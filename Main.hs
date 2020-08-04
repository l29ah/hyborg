{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

import qualified Data.ByteString as B
import Options.Applicative

import Object
import RPC

data Command =
	Create
		{ cProgress :: Bool
		}
	| Info
		{ iRepo :: B.ByteString
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
		) (progDesc "create a new backup"))

main = do
	opts <- execParser $ info optParser (progDesc "borgbackup-compatible backup tool")
	processCommand opts $ oCommand opts

processCommand :: Options -> Command -> IO ()
processCommand opts Info {..} = do
	conn <- openRPC
	negotiate conn
	let repoPath = iRepo	-- TODO parse ::archive
	open conn repoPath True
	manifestData <- get conn repoManifest
	manifest <- readManifest manifestData
	getArchives conn manifest
