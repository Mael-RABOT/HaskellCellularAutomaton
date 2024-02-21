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
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- getArgs
    case (parseOptions args >>= checkArgs) of
        Nothing ->
            hPutStrLn stderr ("Usage: "
                ++ "./wolfram -rule [30|90|110] "
                ++ "(-start [starting line number] -lines [number of lines] "
                ++ "-window [number of cells] -move [number of moves])")
                >> exitWith (ExitFailure 84)
        Just conf -> iterateWorld conf defaultMap
