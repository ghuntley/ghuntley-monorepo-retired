{-# LANGUAGE TemplateHaskell #-}

 -- | Command line interface for @seaweed@ executable.

 module Seaweed.Cli
       ( runSeaweed
       ) where

import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitDirty, gitHash)
import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, help, helper, info,
                            infoOption, long, progDesc, short)

import qualified Paths_seaweed as Meta (version)


runSeaweed :: IO ()
runSeaweed = execParser cliParser

 ----------------------------------------------------------------------------
-- Parsers
----------------------------------------------------------------------------

 -- | Main parser of the app.
cliParser :: ParserInfo ()
cliParser = info ( helper <*> versionP <*> seaweedP )
    $ fullDesc <> progDesc "Seaweed – Create your fancy CV in different formats"

 -- | Commands parser.
seaweedP :: Parser ()
seaweedP = pass

 -- | Show the version of the tool.
versionP :: Parser (a -> a)
versionP = infoOption seaweedVersion
    $ long "version"
   <> short 'v'
   <> help "Show Seaweed's version"

seaweedVersion :: String
seaweedVersion = intercalate "\n"
    $ [sVersion, sHash, sDate] ++ [sDirty | $(gitDirty)]
  where
    sVersion = "Seaweed v." <> showVersion Meta.version
    sHash = " ➤ " <> "Git revision: " <> $(gitHash)
    sDate = " ➤ " <> "Commit date:  " <> $(gitCommitDate)
    sDirty = "There are non-committed files."
