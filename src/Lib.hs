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
import Data.Maybe (fromMaybe)

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
displayWorld (Map left right) (Conf _ _ _ window move) =
    let windowSize = fromMaybe 40 window
        halfWindowSize = windowSize `div` 2
        moveSize = fromMaybe 0 move
        leftPart = reverse $ take (halfWindowSize + moveSize) left
        rightPart = take (windowSize - halfWindowSize - moveSize) right
    in putStrLn $ mapToString (Map (reverse leftPart) rightPart)

rule30 :: Bool -> Bool -> Bool -> Bool
rule30 a b c = a /= (b || c)

rule90 :: Bool -> Bool -> Bool -> Bool
rule90 a _ c = a /= c

rule110 :: Bool -> Bool -> Bool -> Bool
rule110 a b c = (not a && b) || (b && not c) || (not b && c)

chooseRule :: Int -> Bool -> Bool -> Bool -> Bool
chooseRule 30 = rule30
chooseRule 90 = rule90
chooseRule 110  = rule110
chooseRule _ = (\_ b _ -> b)

applyRule :: Int -> Map -> Map
applyRule rule (Map left right) = Map newLeft newRight
  where
    newRight = zipWith3
        (chooseRule rule) (head left : init right) right (tail right)
    newLeft = zipWith3
        (chooseRule rule) (tail left) left (head right : init left)

iterateWorld :: Conf -> Map -> IO ()
iterateWorld (Conf _ _ (Just 0) _ _) _ = return ()
iterateWorld conf@(Conf (Just rule) (Just start) (Just line) d e) world =
    if start > 0 then
        iterateWorld (Conf (Just rule) (Just (start - 1)) (Just line) d e)
            (applyRule rule world)
    else
        displayWorld world conf >>
        iterateWorld (Conf (Just rule) (Just start) (Just(line - 1)) d e)
            (applyRule rule world)
iterateWorld _ _ = return ()
