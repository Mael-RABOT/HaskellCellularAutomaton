{-
-- EPITECH PROJECT, 2024
-- repo
-- File description:
-- file
-}

module Main (main) where

import Lib
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

main :: IO ()
main = do
    args <- getArgs
    case (parseOptions args >>= checkArgs) of
        Nothing -> exitWith (ExitFailure 84)
        Just conf' -> applyRuleNTimes conf' [True]
