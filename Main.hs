{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

import Data.Binary (decode)
import Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Options.Applicative

import Object
import RPC

data Command =
	Create
		{ cProgress :: Bool
		}
	| Info
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
	(command "info" $ info (pure Info) (progDesc "show repo info"))
	<> (command "create" $ info (Create
		<$> switch (long "progress")
		) (progDesc "create a new backup"))

main = do
	opts <- execParser $ info optParser (progDesc "borgbackup-compatible backup tool")
	print opts
	conn <- openRPC
	negotiate conn
	open conn "/home/l29ah/projects/hyborg/test" True
	manifestData <- get conn repoManifest
	manifest <- readManifest manifestData
	getArchives conn manifest
	pure ()
