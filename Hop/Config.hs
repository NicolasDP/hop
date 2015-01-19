-- |
-- Module      : Hop.Config
-- License     : BSD-Style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--

{-# LANGUAGE OverloadedStrings #-}

module Hop.Config
    ( UserConfig(..)
    , Config(..)
    , loadUserConfigFile
    , loadConfigFile
    , initConfigFile
    ) where

import Control.Applicative ((<$>), (<|>))
import Data.String.Parse (Parser, Result(..))
import qualified Data.String.Parse as Parser
import Github.Auth
import System.Directory
import System.FilePath

data UserConfig = UserConfig
    { getUserAuth :: Maybe GithubAuth
    } deriving (Show,Eq)

data Config = Config
    { getProjectOwner :: String
    , getProjectName  :: String
    , getProjectAuth  :: Maybe GithubAuth
    , getProjectPath  :: FilePath
    } deriving (Show, Eq)

gitPath :: FilePath
gitPath = ".git"

gitHopConfigPath :: FilePath
gitHopConfigPath = gitPath </> "hop.conf"

initConfigFile :: IO (Either String ())
initConfigFile = do
    eprojectPath <- lookupProjectPath
    case eprojectPath of
        Left err -> return $ Left err
        Right pp -> do
            let configPath = pp </> gitHopConfigPath
            isExist <- doesFileExist configPath
            if isExist
                then return $ Left $ "init config file error: already exist: " ++ configPath
                else Right <$> promptConfigFile configPath

promptConfigFile :: FilePath -> IO ()
promptConfigFile path = do
    putStrLn $ "update the configuration file: " ++ path
    writeFile path defaultContent

defaultContent :: String
defaultContent =
    "# Hop Configuration file\n"
    ++ "#\n"
    ++ "# Configure the remote information access from here:\n"
    ++ "# * the project's owner\n"
    ++ "# * the project's name\n"
    ++ "# * you github's OAuth string (optional)\n"
    ++ "\n"
    ++ "owner: \"haskell\"\n"
    ++ "\n"
    ++ "repo : \"unix\"\n"
    ++ "\n"
    ++ "# Uncomment the following line and add your OAuth String\n"
    ++ "# You can generate a OAuth String in your Github Settings\n"
    ++ "# oauth: \"your-oauth-string\"\n"

lookupProjectPath :: IO (Either String FilePath)
lookupProjectPath = do
    dir <- getCurrentDirectory
    lookupProjectPath' dir

lookupProjectPath' :: FilePath -> IO (Either String FilePath)
lookupProjectPath' dir = do
    isExist <- doesDirectoryExist $ dir </> gitPath
    if isExist
        then return $ Right dir
        else if parentDir == dir
                then return $ Left "This is not under a git repository"
                else lookupProjectPath' parentDir
  where
    parentDir :: FilePath
    parentDir = takeDirectory dir

loadConfigFile :: IO (Either String Config)
loadConfigFile = do
    eprojectPath <- lookupProjectPath
    case eprojectPath of
        Left  err         -> return $ Left err
        Right projectPath -> parseConfigFile projectPath

loadUserConfigFile :: IO (Either String UserConfig)
loadUserConfigFile = do
    home <- getHomeDirectory
    let configPath = home </> ".hoprc"
    exist <- doesFileExist configPath
    if exist
        then do content <- readFile configPath
                return $ case Parser.parse userConfigFileParser content of
                        ParseOK _ cfg -> Right cfg
                        ParseFail err -> Left $ "error while parsing configuration file " ++ show configPath ++ ": " ++ err
                        ParseMore _   -> Left $ "error while parsing configuration file " ++ show configPath ++ ": Not enough data"
        else return $ Right $ UserConfig
                { getUserAuth = Nothing
                }

parseConfigFile :: FilePath -> IO (Either String Config)
parseConfigFile projectPath = do
    isExist <- doesFileExist configPath
    if isExist
        then do
            content <- readFile configPath
            return $ case Parser.parse (configFileParser projectPath) content of
                ParseOK _ cfg -> Right cfg
                ParseFail err -> Left $ "error while parsing configuration file " ++ show configPath ++ ": " ++ err
                ParseMore _   -> Left $ "error while parsing configuration file " ++ show configPath ++ ": Not enough data"
        else return $ Left $ "This project has no Hop Configuration file in " ++ show projectPath
  where
    configPath :: FilePath
    configPath = projectPath </> gitHopConfigPath

userConfigFileParser :: Parser UserConfig
userConfigFileParser = toUserConfig <$> parseKeyValues
  where toUserConfig l =
            UserConfig
                { getUserAuth = GithubOAuth <$> lookup "oauth" l
                }

configFileParser :: FilePath -> Parser Config
configFileParser projectPath = do
    l <- parseKeyValues
    owner <- maybe (fail "expected Github Project Owner") return $ lookup "owner" l
    repo  <- maybe (fail "expected Github Project Repo")  return $ lookup "repo"  l
    let oauth = GithubOAuth <$> lookup "oauth" l
    return $ Config
        { getProjectOwner = owner
        , getProjectName  = repo
        , getProjectAuth  = oauth
        , getProjectPath  = projectPath
        }

parseKeyValues :: Parser [(String, String)]
parseKeyValues = do
    dropEmptyLinesOrComments <|> return ()
    isEnd <- Parser.isEndOfBuff
    if isEnd
        then return []
        else do
            x  <- parseKeyValue
            xs <- parseKeyValues
            return $ x:xs

many :: Parser a -> Parser [a]
many p = do
    isEnd <- Parser.isEndOfBuff
    if isEnd
        then return []
        else do
            v <- p
            manyAgain v <|> return [v]
  where
    manyAgain v = do
        vs <- many p
        return $ v:vs

dropEmptyLinesOrComments :: Parser ()
dropEmptyLinesOrComments = do
    _ <- many $ (dropComment >> Parser.char '\n') <|> (dropSpaces >> Parser.char '\n')
    return ()

dropSpaces :: Parser ()
dropSpaces = do
    Parser.skipWhile (flip elem " \t")

dropComment :: Parser ()
dropComment = do
    dropSpaces
    Parser.char '#'
    Parser.skipWhile ((/=) '\n')

parseKeyValue :: Parser (String, String)
parseKeyValue = do
    dropSpaces
    key <- parseKey
    dropSpaces
    Parser.char ':'
    dropSpaces
    value <- parseValue
    dropSpaces
    dropComment <|> return ()
    Parser.char '\n'
    return (key, value)

parseKey :: Parser String
parseKey =
    Parser.takeWhile (flip elem (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))

parseValue :: Parser String
parseValue =
    parseQuotedValue <|> parseValueStd

parseValueStd :: Parser String
parseValueStd =
    Parser.takeWhile (flip elem (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_"))

parseQuotedValue :: Parser String
parseQuotedValue = do
    Parser.char '"'
    str <- Parser.takeWhile ((/=) '"')
    Parser.char '"'
    return str
