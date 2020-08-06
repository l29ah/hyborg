{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.DateTime
import Data.List
import Data.String.Class
import Options.Applicative
import Text.Printf

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

connectToRepo :: ByteString -> Bool -> IO (RPCHandle, Manifest)
connectToRepo repoPath rw = do
	conn <- openRPC
	negotiate conn
	open conn repoPath rw
	manifestData <- get conn repoManifest
	manifest <- readManifest manifestData
	pure (conn, manifest)

parseAddress :: ByteString -> (ByteString, ByteString)
parseAddress = (\(repo, archive) -> (repo, B.drop 2 archive)) . B.breakSubstring "::"

processCommand :: Options -> Command -> IO ()
processCommand opts Info {..} = do
	let (repoPath, archiveName) = parseAddress iRepo
	(conn, manifest) <- connectToRepo repoPath False
	getArchives conn manifest
processCommand opts c@Create {..} = do
	print c
	let (repoPath, archiveName) = parseAddress cArchive
	when (B.null repoPath) $ error "no repository specified"
	when (B.null archiveName) $ error "no archive name specified"
	(conn, manifest) <- connectToRepo repoPath True
	mapM_ (\fn -> do
		pure ()
		) cFiles
processCommand opts List {..} = do
	let (repoPath, archiveName) = parseAddress lRepoArchive
	(conn, manifest) <- connectToRepo repoPath False
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
			printf "%6s %6s %-9d %s %s\n" (toString aiUser) (toString aiGroup) aiSize (formatDateTime "%F %X" $ fromSeconds $ fromIntegral $ div aiMTime 1000000000) (toString aiPath)
			) archiveItems
