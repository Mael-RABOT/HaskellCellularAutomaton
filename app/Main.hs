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
    maybe (exitWith (ExitFailure 84)) (print) (parseAndCheckArgs args)
