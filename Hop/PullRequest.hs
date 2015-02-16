-- |
-- Module      : Hop.PullRequest
-- License     : BSD-Style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--

module Hop.PullRequest
    ( listOpenedPullRequests
    , showPullRequest
    , attemptPullRequestTesting
    , reviewPullRequestDiff
    , reviewPullRequestCommits
    , createPullRequestBranch
    , createPullRequestIssue
    ) where

import Console.Color
import Control.Applicative
import Control.Monad
import Data.Char (toLower)
import Github.PullRequests
import Hop.Config
import System.Process
import System.Exit
import Text.Printf

-- | Pretty print a Diff Line
prettyPrintDiffLine :: String -> IO ()
prettyPrintDiffLine [] = putStrLn ""
prettyPrintDiffLine line =
    case head line of
        '+' -> putStrLn $ (console [Green]) ++ line ++ (console [Reset])
        '-' -> putStrLn $ (console [Red]) ++ line ++ (console [Reset])
        _   -> putStrLn line

prettyPrintDiffResume :: File -> IO ()
prettyPrintDiffResume diff = do
    putStrLn $ "File: " ++ (fileFilename diff) ++ " [+/-" ++ (show $ fileChanges diff) ++ "]"
    putStrLn $ "additions: " ++ (console [Green, Bold]) ++ "+++" ++ console [Reset] ++ " " ++ (show $ fileAdditions diff)
    putStrLn $ "deletions: " ++ (console [Red, Bold])   ++ "---" ++ console [Reset] ++ " " ++ (show $ fileDeletions diff)

-- Create Pull Request --------------------------------------------------------

createPullRequestBranch :: Config
                        -> String -- ^ Head
                        -> String -- ^ Base
                        -> String -- ^ Title
                        -> IO ()
createPullRequestBranch cfg headPR basePR title = do
    let auth = maybe (error "You must set the 'oauth' value in your hop file (see hop init)") id $ getProjectAuth cfg
    epr <- createPullRequest auth (getProjectOwner cfg) (getProjectName cfg) $ CreatePullRequest title "" headPR basePR
    case epr of
        Left err -> error $ "create pull request: " ++ show err
        Right pr -> printDetailedPullRequest pr []

createPullRequestIssue :: Config
                       -> Int    -- ^ Issue Number
                       -> String -- ^ Head
                       -> String -- ^ Base
                       -> IO ()
createPullRequestIssue cfg issueNum headPR basePR = do
    let auth = maybe (error "You must set the 'oauth' value in your hop file (see hop init)") id $ getProjectAuth cfg
    epr <- createPullRequest auth (getProjectOwner cfg) (getProjectName cfg) $ CreatePullRequestIssue issueNum headPR basePR
    case epr of
        Left err -> error $ "create pull request: " ++ show err
        Right pr -> printDetailedPullRequest pr []

-- Review the Diff ------------------------------------------------------------

reviewPullRequestDiff :: Config -> Int -> IO ()
reviewPullRequestDiff cfg prNumber = do
    ediffs <- pullRequestFiles' (getProjectAuth cfg) (getProjectOwner cfg) (getProjectName cfg) prNumber
    case ediffs of
        Left  err   -> error $ "review pull request: " ++ show err
        Right diffs -> mapM_ (prettyPrintDiff cfg prNumber) $ zip diffs [0..]

prettyPrintDiff :: Config -> Int -> (File, Int) -> IO ()
prettyPrintDiff cfg prNumber (diff, diffNum) = do
    prettyPrintDiffResume diff
    putStrLn ""
    mapM_ prettyPrintDiffLine $ lines $ filePatch diff
    putStrLn ""
    askForReview
  where
    askForReview :: IO ()
    askForReview = do
        putStrLn "To see or comment this file, follow this URL in your browser: "
        putStrLn $ "  https://github.com/" ++ (getProjectOwner cfg) ++ "/" ++ (getProjectName cfg) ++ "/pull/" ++ show prNumber ++ "/files#diff-" ++ show diffNum
        putStrLn ""
        putStrLn "press [enter] to continue or 'quit' (q, Q or quit)"
        resp <- getLine
        case map toLower resp of
            "quit" -> exitSuccess
            "q"    -> exitSuccess
            _      -> return ()

-- Review the Commits ---------------------------------------------------------

reviewPullRequestCommits :: Config -> Int -> IO ()
reviewPullRequestCommits cfg prNumber = do
    ediffs <- pullRequestCommits' (getProjectAuth cfg) (getProjectOwner cfg) (getProjectName cfg) prNumber
    case ediffs of
        Left  err   -> error $ "review pull request: " ++ show err
        Right diffs -> mapM_ (prettyPrintCommit cfg prNumber) $ zip diffs [0..]

prettyPrintCommit :: Config -> Int -> (Commit, Int) -> IO ()
prettyPrintCommit cfg prNumber (commit, commitNum) = do
    putStrLn $ "Commit: " ++ commitSha commit
    putStrLn $ "Commit URL: " ++ (gitCommitUrl $ commitGitCommit commit)
    case commitCommitter commit of
        Nothing   -> return ()
        Just user -> putStrLn $ "Committer: " ++ githubOwnerLogin user
    case commitAuthor commit of
        Nothing   -> return ()
        Just user -> putStrLn $ "Author: " ++ githubOwnerLogin user
    putStrLn ""
    mapM_ putStrLn $ lines $ gitCommitMessage $ commitGitCommit commit
    putStrLn ""
    putStrLn "q, Q, quit or QUIT: exit the commit review"
    putStrLn "y, Y, yes or YES: see the commit Diff"
    putStrLn "nothing or anything else: see the next commit"
    resp <- getLine
    case map toLower resp of
        "y"   -> prettyPrintCommitDiffs cfg commit
        "yes" -> prettyPrintCommitDiffs cfg commit
        "q"    -> exitSuccess
        "quit" -> exitSuccess
        _ -> return ()

prettyPrintCommitDiffs :: Config -> Commit -> IO ()
prettyPrintCommitDiffs cfg commit = do
    print commit
    prettyPrintCommitDiffs' $ zip (commitFiles commit) [0..]
  where
    prettyPrintCommitDiffs' :: [(File, Int)] -> IO ()
    prettyPrintCommitDiffs' [] = return ()
    prettyPrintCommitDiffs' (x:xs) = do
        printNext <- prettyPrintCommitDiff cfg commit x
        if printNext
            then prettyPrintCommitDiffs' xs
            else return ()

prettyPrintCommitDiff :: Config -> Commit -> (File, Int) -> IO Bool
prettyPrintCommitDiff cfg commit (diff, diffNum) = do
    prettyPrintDiffResume diff
    putStrLn ""
    mapM_ prettyPrintDiffLine $ lines $ filePatch diff
    putStrLn ""
    askForReview
  where
    askForReview :: IO Bool
    askForReview = do
        putStrLn "To see this file in the commit context follow this URL in your browser: "
        putStrLn $ "  https://github.com/" ++ (getProjectOwner cfg) ++ "/" ++ (getProjectName cfg) ++ "/commit/" ++ commitSha commit ++ "#diff-" ++ show diffNum
        putStrLn ""
        putStrLn "press [enter] to continue or 'quit' (q, Q or quit)"
        resp <- getLine
        case map toLower resp of
            "quit" -> return False
            "q"    -> return False
            _      -> return True

-- Try the pull request -------------------------------------------------------

isItSafe :: IO Bool
isItSafe = do
    (returnCode, stdoutput, stderror) <- readProcessWithExitCode "git" ["diff"] []
    case returnCode of
        ExitSuccess   -> return (null stdoutput && null stderror)
        ExitFailure i -> error ("\"git diff\" exit with " ++ show i ++ "\n\n" ++ stderror)

tryCommand :: String -> [String] -> IO (Either String ())
tryCommand cmd opt = do
    (returnCode, _, stderror) <- readProcessWithExitCode cmd opt []
    case returnCode of
        ExitSuccess   -> return $ Right ()
        ExitFailure i -> return $ Left ("\"" ++ cmd ++ " " ++ (show opt) ++"\" exit with " ++ show i ++ "\n\n" ++ stderror)

fetchBranch :: String -> String -> IO ()
fetchBranch remote branch = do
    putStrLn $ " * Fetch remote branch: " ++ remote ++ "/" ++ branch
    either error id <$> tryCommand "git" ["fetch", remote, branch]

createOrUpdateBranch :: String -> String -> IO String
createOrUpdateBranch remote branch = do
    putStrLn $ " * Create or update Pull Request branch: " ++ remoteBranch
    fetchBranch remote branch
    either error id <$> tryCommand "git" ["checkout", remoteBranch, "-B", newBranch]
    return newBranch
  where
    newBranch :: String
    newBranch = "hop/" ++ branch
    remoteBranch :: String
    remoteBranch = remote ++ "/" ++ branch

rebaseBranchOnBranch :: (String, String) -> String -> IO ()
rebaseBranchOnBranch (upstreamRemote, upstreamBranch) branch = do
    putStrLn $ " * Rebase branch " ++ show branch ++ " on " ++ upstreamRemoteBranch
    either error id <$> tryCommand "git" ["rebase", upstreamRemoteBranch, branch]
  where
    upstreamRemoteBranch :: String
    upstreamRemoteBranch = upstreamRemote ++ "/" ++ upstreamBranch

getCommitInfo :: Config -> Int -> IO DetailedPullRequest
getCommitInfo cfg prNumber = do
    epr <- pullRequest' (getProjectAuth cfg) (getProjectOwner cfg) (getProjectName cfg) prNumber
    case epr of
        Left  err -> error $ "getCommitInfo: " ++ show err
        Right pr  -> return pr

attemptPullRequestTesting :: Config -> Int -> Bool -> IO ()
attemptPullRequestTesting cfg prNumber rebase = do
    isSafe <- isItSafe
    case isSafe of
        False -> do
            printf
                "%s%s%sSave your changes before going further (see `git diff`)\n"
                (console [Green, Red])
                "your working directory is not clean\n"
                (console [Reset])
        True  -> do
            pr <- getCommitInfo cfg prNumber
            let baseBranch = pullRequestCommitRef $ detailedPullRequestBase pr
            let prBranch   = pullRequestCommitRef $ detailedPullRequestHead pr
            -- Make sure Master ref is up to date
            when rebase $ fetchBranch "origin" baseBranch
            -- create a new branch and checkout on it for the test
            nbranch <- createOrUpdateBranch "origin" prBranch
            -- rebase the branch
            when rebase $ rebaseBranchOnBranch ("origin", baseBranch) nbranch

-- List Opened Pull requests --------------------------------------------------

listOpenedPullRequests :: Config -> IO ()
listOpenedPullRequests cfg = do
    eprs <- pullRequestsFor' (getProjectAuth cfg) (getProjectOwner cfg) (getProjectName cfg)
    case eprs of
        Left  err -> error $ "listOpendedPullRequests: error: " ++ show err
        Right prs -> do
            putStrLn $ getProjectOwner cfg ++ "/" ++ getProjectName cfg ++ ": active Pull Requests"
            printPullRequestsInfo "  " prs

printPullRequestsInfo :: String -> [PullRequest] -> IO ()
printPullRequestsInfo _      [] = return ()
printPullRequestsInfo indent (pr:prs) = do
    printPullRequestInfo indent pr
    printPullRequestsInfo indent prs

printPullRequestInfo :: String -> PullRequest -> IO ()
printPullRequestInfo indent pr = do
    printf
        "%s[%u] %s%-16s%s %s\n"
        (indent)
        (pullRequestNumber pr)
        (console [Bold, Blue])
        (githubOwnerLogin $ pullRequestUser pr)
        (console [Reset])
        (pullRequestTitle pr)

-- Show a Pull Request --------------------------------------------------------

showPullRequest :: Config -> Int -> IO ()
showPullRequest cfg prNumber = do
    pr    <- either printError id <$> pullRequest' (getProjectAuth cfg) (getProjectOwner cfg) (getProjectName cfg) prNumber
    diffs <- either printError id <$> pullRequestFiles' (getProjectAuth cfg) (getProjectOwner cfg) (getProjectName cfg) prNumber
    printDetailedPullRequest pr diffs
  where
    printError err = error $ "showPullRequest: error: " ++ show err

printDetailedPullRequest :: DetailedPullRequest -> [File] -> IO ()
printDetailedPullRequest pr l = do
    -- print the basis information
    printf
        "[%u](%s) %s%s\n"
        (detailedPullRequestNumber pr)
        (detailedPullRequestState pr)
        (detailedPullRequestTitle pr)
        (if (detailedPullRequestMerged pr) then " [merged]" else "")
    -- print status information
    printf "  Opened by %s%s%s\n" (console [Bold, Blue]) (githubOwnerLogin $ detailedPullRequestUser pr) (console [Reset])
    printf "  Opened  %s\n" (show $ fromGithubDate $ detailedPullRequestCreatedAt pr)
    printf "  Updated %s\n" (show $ fromGithubDate $ detailedPullRequestUpdatedAt pr)
    case detailedPullRequestMergedAt pr of
        Nothing -> return ()
        Just d  -> printf "  Merged  %s%s%s\n" (console [Bold]) (show $ fromGithubDate d) (console [Reset])
    case detailedPullRequestMergedBy pr of
        Nothing -> return ()
        Just u  -> printf "  Merged by %s%s%s\n" (console [Bold, Blue]) (githubOwnerLogin u) (console [Reset])
    case detailedPullRequestClosedAt pr of
        Nothing -> return ()
        Just d  -> printf "  Closed  %s%s%s\n" (console [Bold]) (show $ fromGithubDate d) (console [Reset])
    printf "\n"
    -- print pull request description
    printf
        "Infos%s: %u commit(s), %u changed file(s), %s+++%s %u %s---%s %u\n"
        (maybe "" (\b -> if b then " [mergeable]" else " [" ++ console [Bold, Green] ++ "NOT-mergeable" ++ console [Reset] ++ "]") $ detailedPullRequestMergeable pr)
        (detailedPullRequestCommits pr)
        (detailedPullRequestChangedFiles pr)
        (console [Green]) (console [Reset]) (detailedPullRequestAdditions pr)
        (console [Red])   (console [Reset]) (detailedPullRequestDeletions pr)
    mapM_ printDiffsFileAndChanges l
    printf "\n"
    -- print general info
    printf "Description:\n%s\n" $
        if null $ detailedPullRequestBody pr
            then "  <no-description-provided>"
            else (unlines $ map (\str -> "  " ++ str) $ lines $ detailedPullRequestBody pr)

printDiffsFileAndChanges :: File -> IO ()
printDiffsFileAndChanges diff = do
    printf
        "  %s%s%s%-4s %s%s%s%-4s  %s%s%s\n"
        (console [Green])
        (if null numberOfAdd then "   " else "+++")
        (console [Reset])
        numberOfAdd

        (console [Red])
        (if null numberOfDel then "   " else "---")
        (console [Reset])
        (numberOfDel)

        (console $ getAdaptedColor $ fileStatus diff)
        (fileFilename diff)
        (console [Reset])
  where
    numberOfAdd :: String
    numberOfAdd = show $ fileAdditions diff

    numberOfDel :: String
    numberOfDel = show $ fileDeletions diff

    getAdaptedColor :: String -> [ShellCode]
    getAdaptedColor status =
        case status of
            "removed"  -> [Red]
            "added"    -> [Green]
            "modified" -> [Reset]
            _          -> [Reset]
