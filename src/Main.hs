-- |
-- Module      : Main
-- License     : BSD-Style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--

module Main
    ( main
    ) where

import Control.Applicative
import Hop.Config
import Hop.PullRequest
import System.Environment

startCommand :: [String] -> IO ()
startCommand args = do
    userCfg <- either error id <$> loadUserConfigFile
    projCfg <- either error id <$> loadConfigFile

    -- if project auth is not present, replace it by the user auth.
    let cfg = case getProjectAuth projCfg of
                Nothing -> projCfg { getProjectAuth = getUserAuth userCfg }
                Just _  -> projCfg

    case args of
        "list":[] -> listOpenedPullRequests cfg
        "show":num:[] -> showPullRequest cfg (read num)
        "try":num:[]               -> attemptPullRequestTesting cfg (read num) True
        "try":"--no-rebase":num:[] -> attemptPullRequestTesting cfg (read num) False
        "review":num:[]           -> reviewPullRequestDiff cfg (read num)
        "review":"diff":num:[]    -> reviewPullRequestDiff cfg (read num)
        "review":"commits":num:[] -> reviewPullRequestCommits cfg (read num)
        "create":headPR:title:[]        -> createPullRequestBranch cfg headPR "master" title
        "create":"--issue":issue:headPR:[]        -> createPullRequestIssue cfg (read issue) headPR "master"
        "create":headPR:basePR:title:[] -> createPullRequestBranch cfg headPR basePR   title
        "create":"--issue":issue:headPR:basePR:[] -> createPullRequestIssue cfg (read issue) headPR basePR
        _         -> startHelpCommand $ Just $ "command not found: " ++ show args ++ "\n"

startInitConfig :: IO ()
startInitConfig = do
    e <- initConfigFile
    case e of
        Left err -> error err
        Right _  -> return ()

startHelpCommand :: Maybe String -> IO ()
startHelpCommand comment = do
    maybe (return ()) putStrLn comment
    putStrLn "Hop: Github collaboration project tool"
    putStrLn ""
    putStrLn "Usage:"
    putStrLn "  hop help"
    putStrLn "  hop init"
    putStrLn "  hop <command>"
    putStrLn ""
    putStrLn "Commands:"
    putStrLn "  help: print this help message"
    putStrLn "  init: initialize the default configuration file (you may need to update it)"
    putStrLn "  list: list all the current pull request on the project"
    putStrLn "  show <PullRequestNumber>: show a given pull request"
    putStrLn "  try [--no-rebase] <PullRequestNumber>: create a new branch for the pull request and rebase it on master (unless option --no-rebase)"
    putStrLn "  review [diff] <PullRequestNumber>: review the diff"
    putStrLn "  review commits <PullRequestNumber>: review the commits"
    putStrLn "  create <HeadPR> [basePR] <PullRequestTitle>: create a pull request"
    putStrLn "  create --issue <issueNumber> <HeadPR> [basePR]: create a pull request from an issue"

main :: IO ()
main = do
    args <- getArgs
    case args of
        "init":[] -> startInitConfig
        "help":[] -> startHelpCommand Nothing
        _         -> startCommand args
