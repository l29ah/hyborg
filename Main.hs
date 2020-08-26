{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.DateTime
import Data.Default
import Data.List
import Data.String.Class
import Data.Word
import Foreign.C.Types
import Options.Applicative
import System.Posix.Files
import System.Posix.IO
import Text.Printf

import Cache
import Chunker
import Object
import RPC
import Types

data Command =
	Create
		{ cProgress :: Bool
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
	, oCommand :: Command
	} deriving (Eq, Show)

optParser :: Parser Options
optParser = Options
	<$> switch (short 'v' <> long "verbose" <> long "info")
	<*> commandParser

commandParser :: Parser Command
commandParser = hsubparser $
	(command "info" $ info (Info
		<$> strArgument (metavar "REPOSITORY_OR_ARCHIVE" <> help "repository/archive address")
		) (progDesc "show repo info"))
	<> (command "create" $ info (Create
		<$> switch (long "progress")
		<*> strArgument (metavar "ARCHIVE" <> help "archive name")
		<*> some (strArgument (metavar "PATH..." <> help "file path"))
		) (progDesc "create a new backup"))
	<> (command "list" $ info (List
		<$> strArgument  (metavar "REPOSITORY_OR_ARCHIVE" <> help "repository/archive address")
		) (progDesc "list archives or files"))

main = do
	opts <- execParser $ info optParser (progDesc "borgbackup-compatible backup tool")
	processCommand opts $ oCommand opts

connectToRepo :: ByteString -> Bool -> IO (RPCHandle, ID Repository, Manifest)
connectToRepo repoPath rw = do
	conn <- openRPC
	negotiate conn
	id <- open conn repoPath rw
	manifestData <- get conn repoManifest
	manifest <- readManifest manifestData
	pure (conn, id, manifest)

parseAddress :: ByteString -> (ByteString, ByteString)
parseAddress = (\(repo, archive) -> (repo, B.drop 2 archive)) . B.breakSubstring "::"

-- TODO fetch nanoseconds from fstat(2)
toNanoSeconds :: CTime -> Word64
toNanoSeconds (CTime t) = 1000000000 * fromIntegral t

processCommand :: Options -> Command -> IO ()
processCommand opts Info {..} = do
	let (repoPath, archiveName) = parseAddress iRepo
	(conn, _, manifest) <- connectToRepo repoPath False
	getArchives conn manifest
processCommand opts c@Create {..} = do
	print c
	let (repoPath, archiveName) = parseAddress cArchive
	when (B.null repoPath) $ error "no repository specified"
	when (B.null archiveName) $ error "no archive name specified"
	(conn, id, manifest) <- connectToRepo repoPath True
	fileCache <- readCache id
	mapM_ (\fn -> bracket (openFd fn ReadOnly Nothing defaultFileFlags) closeFd $ \fd -> do
		status <- getFdStatus fd
		-- TODO check if the file is backed up already via cache
		chunks <- chunkifyFile def fd	-- TODO settable chunker settings
		let chunkIDs = []	-- TODO
		let group = ""	-- TODO
		let owner = ""	-- TODO
		let ai = ArchiveItem chunkIDs
			(toNanoSeconds $ accessTime status)
			(toNanoSeconds $ statusChangeTime status)
			(toNanoSeconds $ modificationTime status)
			(fromIntegral $ fileGroup status) group
			(fromIntegral $ fileOwner status) owner
			True
			(fromString fn)
			(fromIntegral $ fileSize status)
		print ai
		pure ()
		) cFiles
processCommand opts List {..} = do
	let (repoPath, archiveName) = parseAddress lRepoArchive
	(conn, _, manifest) <- connectToRepo repoPath False
	if (B.null archiveName) then do
		-- list all the archives present in the repo
		let archs = listArchives manifest
		mapM_ (\(archiveName, archiveID, time) -> do
			printf "%-36s %s [%s]\n" (toString archiveName) (toString time) (toString archiveID)
			) archs
	else do
		-- list files in the archive
		let archiveID = (\(_, id, _) -> id) $ maybe (error $ "archive " ++ show archiveName ++ " not found in the repo") id $ find (\(name, _, _) -> name == archiveName) $ listArchives manifest
		arch <- getArchive conn archiveID
		archiveItems <- mapM (getArchiveItem conn) $ archiveItems arch
		mapM_ (\ArchiveItem {..} ->
			printf "%6s %6s %-9d %s %s\n" (toString user) (toString group) size (formatDateTime "%F %X" $ fromSeconds $ fromIntegral $ div mtime 1000000000) (toString path)
			) archiveItems
