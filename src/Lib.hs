{-
-- EPITECH PROJECT, 2024
-- repo
-- File description:
-- file
-}

module Lib
    ( parseOptions
    , checkArgs
    , parseAndCheckArgs
    ) where

import System.Console.GetOpt
import Text.Read (readMaybe)

data Conf = Conf (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int)
    deriving Show

defaultConf :: Conf
defaultConf = Conf Nothing (Just 0) Nothing (Just 80) Nothing

data Flag = Rule String | Start String | Lines String | Window String | Move String

options :: [OptDescr Flag]
options =
    [ Option [] ["rule"] (ReqArg Rule "<int>") "Option rule"
    , Option [] ["start"] (ReqArg Start "<int>") "Option start"
    , Option [] ["lines"] (ReqArg Lines "<int>") "Option lines"
    , Option [] ["window"] (ReqArg Window "<int>") "Option window"
    , Option [] ["move"] (ReqArg Move "<int>") "Option move"
    ]

parseOptions :: [String] -> Maybe Conf
parseOptions argv =
    case getOpt Permute options argv of
        (flags, _, []) -> Just $ foldl (flip setFlag) defaultConf flags
        (flags, _, errs) -> if not (null errs)
                        then Nothing
                        else Just $ foldl (flip setFlag) defaultConf flags


setFlag :: Flag -> Conf -> Conf
setFlag (Rule arg) (Conf _ b c d e) = Conf (readMaybe arg) b c d e
setFlag (Start arg) (Conf a _ c d e) = Conf a (readMaybe arg) c d e
setFlag (Lines arg) (Conf a b _ d e) = Conf a b (readMaybe arg) d e
setFlag (Window arg) (Conf a b c _ e) = Conf a b c (readMaybe arg) e
setFlag (Move arg) (Conf a b c d _) = Conf a b c d (readMaybe arg)

checkArgs :: Conf -> Maybe Conf
checkArgs (Conf Nothing _ _ _ _) = Nothing
checkArgs conf = Just conf

parseAndCheckArgs :: [String] -> Maybe Conf
parseAndCheckArgs argv = do
    conf <- parseOptions argv
    checkArgs conf
