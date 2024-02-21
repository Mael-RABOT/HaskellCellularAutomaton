{-
-- EPITECH PROJECT, 2024
-- repo
-- File description:
-- file
-}

module Lib
    ( parseOptions
    , checkArgs
    , defaultMap
    , Conf(..)
    , Map(..)
    , displayWorld
    , iterateWorld
    ) where

import Text.Read (readMaybe)

data Conf = Conf (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int)
    deriving Show

defaultConf :: Conf
defaultConf = Conf Nothing (Just 0) Nothing (Just 80) (Just 0)

f :: Conf -> [String] -> Maybe Conf
f conf [] = Just conf
f (Conf _ b c d e) ("--rule":arg:rest) =f (Conf (readMaybe arg) b c d e) rest
f (Conf a _ c d e) ("--start":arg:rest) = f (Conf a (readMaybe arg) c d e) rest
f (Conf a b _ d e) ("--lines":arg:rest) = f (Conf a b (readMaybe arg) d e) rest
f (Conf a b c _ e) ("--window":arg:rest) =
    f (Conf a b c (readMaybe arg) e) rest
f (Conf a b c d _) ("--move":arg:rest) = f (Conf a b c d (readMaybe arg)) rest
f _ _ = Nothing

parseOptions :: [String] -> Maybe Conf
parseOptions argv = f defaultConf argv

checkArgs :: Conf -> Maybe Conf
checkArgs (Conf (Just rule) start line window move)
    | rule `elem` [30, 90, 110] =
        Just (Conf (Just rule) start line window move)
    | otherwise = Nothing
checkArgs _ = Nothing


data Map = Map [Bool] [Bool]
    deriving Show

defaultMap :: Map
defaultMap = Map (repeat False) (True : repeat False)

mapToString :: Map -> String
mapToString (Map left right) = map boolToChar (reverse left ++ right)
  where
    boolToChar True = '*'
    boolToChar False = ' '

displayWorld :: Map -> Conf -> IO ()
displayWorld (Map left right) (Conf _ _ _ window _) =
    let windowSize = maybe 40 (`div` 2) window
        windowSizeAdjusted = if even windowSize
            then windowSize - 1 else windowSize
        leftPart = reverse $ take windowSizeAdjusted left
        rightPart = take windowSize right
    in putStrLn $ mapToString (Map leftPart rightPart)

rule30 :: [Bool] -> [Bool]
rule30 xs = map rule30Cell (zip3 (False:xs) xs (tail xs ++ [False]))
  where
    rule30Cell (a, b, c) = a /= (b || c)

rule90 :: [Bool] -> [Bool]
rule90 xs = map rule90Cell (zip3 (False:xs) xs (tail xs ++ [False]))
  where
    rule90Cell (a, _, c) = a /= c

rule110 :: [Bool] -> [Bool]
rule110 xs = map rule110Cell (zip3 (False:xs) xs (tail xs ++ [False]))
  where
    rule110Cell (a, b, c) = (not a && b) || (b && not c) || (not b && c)

chooseRule :: Int -> [Bool] -> [Bool]
chooseRule 30 = rule30
chooseRule 90 = rule90
chooseRule 110  = rule110
chooseRule _ = error "Invalid rule"

applyRule :: Int -> Map -> Map
applyRule rule (Map left right) =
    let ruleFunc = chooseRule rule
        newLeft = ruleFunc left
        newRight = ruleFunc right
    in Map newLeft newRight

iterateWorld :: Conf -> Map -> IO ()
iterateWorld (Conf _ _ (Just 0) _ _) _ = return ()
iterateWorld conf@(Conf (Just rule) b (Just line) d e) world =
    displayWorld world conf >>
    iterateWorld (Conf (Just rule) b (Just(line - 1)) d e)
        (applyRule rule world)
iterateWorld _ _ = return ()
