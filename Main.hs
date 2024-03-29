{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables, RecordWildCards, Strict, CPP #-}
#if __GLASGOW_HASKELL__ < 902
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
#else
{-# LANGUAGE OverloadedRecordDot #-}
#endif
{-# LANGUAGE DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, GADTs #-}

import Control.Concurrent
import Control.Concurrent.ParallelIO.Local
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Coerce
import Data.DateTime
import Data.Default
import Data.List
import qualified Data.Map as M
import Data.String.Class hiding (concat, null)
import qualified Data.Time as UTC
import Data.Word
import Data.MessagePack.Types
import Foreign.C.Types
import Options.Applicative
import Prelude hiding (putStrLn)
import System.Directory hiding (isSymbolicLink)
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types
import System.Posix.User
import Text.Printf

import Cache
import Chunker
import Object
import RPC
import Types

data Command =
	Create
		{ cProgress :: Bool
		, cList :: Bool
		, cArchive :: B.ByteString
		, cFiles :: [FilePath]
		}
	| Info
		{ iRepo :: B.ByteString
		}
	| List
		{ lRepoArchive :: B.ByteString
		}
	deriving (Eq, Show)

data Options = Options
	{ oVerbose :: Bool
	, oDryRun :: Bool
	, oCommand :: Command
	} deriving (Eq, Show)

optParser :: Parser Options
optParser = Options
	<$> switch (short 'v' <> long "verbose" <> long "info")
	<*> switch (long "dry-run" <> help "do not connect to the daemon, only test the (FIXME non-lazy part of the) archiving routine")
	<*> commandParser

commandParser :: Parser Command
commandParser = hsubparser $
	(command "info" $ info (Info
		<$> strArgument (metavar "REPOSITORY_OR_ARCHIVE" <> help "repository/archive address")
		) (progDesc "show repo info"))
	<> (command "create" $ info (Create
		<$> switch (long "progress")
		<*> switch (long "list")
		<*> strArgument (metavar "ARCHIVE" <> help "archive name")
		<*> some (strArgument (metavar "PATH..." <> help "file path"))
		) (progDesc "create a new backup"))
	<> (command "list" $ info (List
		<$> strArgument  (metavar "REPOSITORY_OR_ARCHIVE" <> help "repository/archive address")
		) (progDesc "list archives or files"))

main = do
	opts <- execParser $ info optParser (progDesc "borgbackup-compatible backup tool")
	processCommand opts $ oCommand opts

connectToRepo :: ByteString -> Bool -> Bool -> IO (RPCHandle, ID Repository, Manifest)
connectToRepo repoPath rw False = do
	conn <- openRPC repoPath
	id <- open conn repoPath rw
	manifestData <- get conn repoManifest
	manifest <- readManifest manifestData
	pure (conn, id, manifest)
connectToRepo repoPath rw True = do
	conn <- openPseudoRPC
	pure (conn, coerce repoManifest, def)

parseAddress :: ByteString -> (ByteString, ByteString)
parseAddress = (\(repo, archive) -> (repo, B.drop 2 archive)) . B.breakSubstring "::"

-- TODO fetch nanoseconds from fstat(2)
toNanoSeconds :: CTime -> Word64
toNanoSeconds (CTime t) = 1000000000 * fromIntegral t

-- |borg doesn't like absolute paths
stripSlash ('/':xs) = xs
stripSlash xs = xs

archiveChunk :: RPCHandle -> CryptoMethod -> BL.ByteString -> IO DescribedChunk
archiveChunk conn encryption chunk = do
	let strictChunk = BL.toStrict chunk
	let chunkUncompressedSize = (fromIntegral . B.length) strictChunk
	let chunkID = (coerce . encryption.hashID) strictChunk
	let compressedChunk = (BL.toStrict . Object.encrypt encryption) chunk
	let chunkCompressedSize = (fromIntegral . B.length) compressedChunk
	let chunkMeta = DescribedChunk chunkID chunkUncompressedSize chunkCompressedSize
	-- store the chunk data on the server
	cachedPut conn (compressedChunk, chunkMeta.chunkID)
	pure chunkMeta

archiveDir conn cmd chunkerSettings encryption fn tg = do
	-- TODO maybe fdreaddir after https://github.com/haskell/unix/pull/110 is in
	files <- listDirectory fn
	archiveFiles conn cmd chunkerSettings encryption (map (\file -> fn ++ "/" ++ file) files) tg

mapMTasks tg fun list = parallel tg $ map fun list

archiveFiles conn cmd chunkerSettings encryption filenames tg = do
	archiveItemIDs <- mapMTasks tg (\fn -> bracket (openFd fn ReadOnly Nothing defaultFileFlags) closeFd $ \fd -> do
		status <- getSymbolicLinkStatus fn
		when (cmd.cList) $ putStrLn $ "A " ++ fn
		-- |archive a chunked directory entry
		let	archiveEntry :: [BL.ByteString] -> Maybe FilePath -> IO (ID ArchiveItem)
			archiveEntry chunks maybeSource = do
				-- store the file data on the server
				-- TODO check if parallelism makes sense here
				describedChunks <- mapM (archiveChunk conn encryption) chunks

				let gid = fileGroup status
				group <- groupName `fmap` (getGroupEntryForID $ coerce gid)
				let uid = fileOwner status
				owner <- userName `fmap` (getUserEntryForID $ coerce uid)
				let hlid = if (linkCount status > 1) then Just $ hardlinkIdFromInode (fileID status) (deviceID status) else Nothing

				let ai = ArchiveItem
					(toNanoSeconds $ accessTime status)
					(toNanoSeconds $ statusChangeTime status)
					(toNanoSeconds $ modificationTime status)
					(fromIntegral gid) (fromString group)
					(fromIntegral uid) (fromString owner)
					(coerce $ fileMode status)
					hlid
					(fromString $ stripSlash fn)
					(fromIntegral $ fileSize status)
					(if null describedChunks then Nothing else Just describedChunks)
					maybeSource
				let (sai, archiveItemID) = Object.serialize encryption ai
				cachedPut conn (sai, archiveItemID)
				pure $ coerce archiveItemID
		if isDirectory status then do
			-- archive the directory contents
			contents <- archiveDir conn cmd chunkerSettings encryption fn tg
			-- and then the directory itself
			directory <- archiveEntry [] Nothing
			pure $ directory:contents
		else if isSymbolicLink status then do
			contents <- readSymbolicLink fn
			file <- archiveEntry [] $ Just contents
			pure [file]
		else do
			-- TODO check if the file is backed up already via cache
			chunks <- chunkifyFile chunkerSettings fd
			file <- archiveEntry chunks Nothing
			pure [file]
		) filenames
	pure $ concat archiveItemIDs

processCommand :: Options -> Command -> IO ()
processCommand opts Info {..} = do
	let (repoPath, archiveName) = parseAddress iRepo
	(conn, _, manifest) <- connectToRepo repoPath False opts.oDryRun
	getArchives conn manifest
processCommand opts c@Create {..} = do
	let (repoPath, archiveName) = parseAddress cArchive
	when (B.null repoPath) $ error "no repository specified"
	when (B.null archiveName) $ error "no archive name specified"
	(conn, id, manifest) <- connectToRepo repoPath True opts.oDryRun
	when (M.member archiveName manifest.archives) $ error $ "archive " ++ toString archiveName ++ " already exists"
	timeBegin <- UTC.getCurrentTime
	--fileCache <- readCache id
	let chunkerSettings = def	-- TODO settable chunker settings
	let encryption = plaintext	-- TODO other encryption schemes
	nproc <- getNumCapabilities
	archiveItemIDs <- withPool nproc $ archiveFiles conn c chunkerSettings encryption cFiles	-- TODO user-settable number of threads
	timeEnd <- UTC.getCurrentTime
	let arch = Archive
		{ chunkerParams = (fromIntegral chunkerSettings.minExp, fromIntegral chunkerSettings.maxExp, fromIntegral chunkerSettings.maskBits, fromIntegral chunkerSettings.windowSize)
		, cmdline = ["TODO"]
		, comment = "TODO"
		, hostname = "TODO"
		, items = archiveItemIDs
		, name = archiveName
		, tam = def	-- TODO good one in case of encrypted backups
		, time = timeBegin
		, timeEnd = timeEnd
		, username = "TODO"
		, version = 1
		}
	let (serializedArch, archID) = Object.serialize encryption arch
	cachedPut conn (serializedArch, archID)
	let archiveDesc = DescribedArchive
		{ _id = archID
		, time = timeBegin
		}
	-- TODO preserve the ordering of the existing archive entries?
	let newManifest = manifest{archives = M.insert archiveName archiveDesc manifest.archives, timestamp = timeEnd}
	authenticatedManifest <- addTAMm newManifest
	cachedPut conn (fst $ Object.serialize encryption authenticatedManifest, repoManifest)
	commit conn
processCommand opts List {..} = do
	let (repoPath, archiveName) = parseAddress lRepoArchive
	(conn, _, manifest) <- connectToRepo repoPath False opts.oDryRun
	if (B.null archiveName) then do
		-- list all the archives present in the repo
		let archs = listArchives manifest
		mapM_ (\(archiveName, archiveID, time) -> do
			printf "%-36s %s [%s]\n" (toString archiveName) (show time) (toString archiveID)
			) archs
	else do
		-- list files in the archive
		let archiveID = (\(_, id, _) -> id) $ maybe (error $ "archive " ++ show archiveName ++ " not found in the repo") id $ find (\(name, _, _) -> name == archiveName) $ listArchives manifest
		arch <- getArchive conn archiveID
		archiveItems <- mapM (getArchiveItem conn) $ arch.items
		mapM_ (\ArchiveItem {..} ->
			printf "%6s %6s %-9d %s %s\n" (toString user) (toString group) size (formatDateTime "%F %X" $ fromSeconds $ fromIntegral $ div mtime 1000000000) (toString path)
			) archiveItems
