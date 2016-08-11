module Main (main) where

import Data.Monoid ((<>))
import Control.Monad (unless)

import qualified Console.Options as C
import Development.Shake
import Development.Shake.FilePath
import Data.Version (makeVersion)

runShake :: ShakeOptions -> String -> String -> String -> IO ()
runShake opt cmdTarget branchBase branchTarget = shake opt $ do
    want [cmdTarget]
    phony "fetch" $ cmd "git" ["fetch"]
    phony "checkout" $ do
        need ["fetch"]
        cmd "git" ["checkout", branchTarget, "-B", "shake" </> branchTarget </> branchBase ]
    phony "merge" $ do
        need ["checkout"]
        cmd "git" ["merge", branchBase]

    "cpplint.py" %> \cpplint -> do
       unit $ cmd "curl" ["https://raw.githubusercontent.com/google/styleguide/gh-pages/cpplint/cpplint.py", "-o", cpplint]
       unit $ cmd "chmod" ["+x", cpplint]
    phony "cppcheck" $ do
        Exit c <- cmd "which" "cppcheck"
        putNormal $ "which cppcheck returned " ++ show c

    phony "lint-check" $ do
        need ["merge", "cppcheck"]
        Stdout out <- cmd "git" ["diff", "--name-only", branchBase, branchTarget]
        cmd "cppcheck" (lines out)
    phony "lint-style" $ do
        need ["merge", "cpplint.py"]
        Stdout out <- cmd "git" ["diff", "--name-only", branchBase, branchTarget]
        cmd "./cpplint.py" (lines out)
    "build_dir/Makefile" %> \_ -> do
        b <- doesDirectoryExist "build_dir"
        unless b $ cmd "mkdir" ["build_dir"]
        cmd (Cwd "build_dir") "cmake" [".."]
    phony "build" $ do
        need ["merge", "build_dir/Makefile"]
        cmd (Cwd "build_dir") "make" ["-j8"]
    phony "test" $ do
        need ["build"]
        cmd (Cwd "build_dir") "make" ["test"]
    phony "shake" $ do
        need ["lint-style", "lint-check", "test"]
        return ()

main :: IO ()
main = C.defaultMain $ do
    C.programName "hop"
    C.programVersion $ makeVersion [1, 0, 0]
    C.programDescription $ concat
        [ "Hop is a command line tool to help manage review of pull request "
        , "for C++ developpers..."
        ]
    -- TODO: get the git directory
    verbose <- C.flag $ C.FlagShort 'v' <> C.FlagLong "verbose"
    C.command "shake" $ do
        refBaseParam  <- C.argument "ref-base" Right
        refTargetParam <- C.remainingArguments "ref-target"
        C.action $ \toParam -> do
            let refBase    = toParam refBaseParam
            let refTargets = toParam refTargetParam
            let refTarget  = case refTargets of
                                     []  -> "master"
                                     [x] -> x
                                     _   -> error $ "too many ref targer: " ++ show refTargets
            let shakeopts = shakeOptions { shakeVerbosity =  if toParam verbose then Chatty else Normal }
            print refBase
            print refTarget
            runShake shakeopts "shake" refBase refTarget

