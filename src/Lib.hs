{-
-- EPITECH PROJECT, 2024
-- repo
-- File description:
-- file
-}

module Lib
    ( parseOptions
    , checkArgs
    , applyRuleNTimes
    ) where

import Text.Read (readMaybe)

data Conf = Conf (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int)
    deriving Show

defaultConf :: Conf
defaultConf = Conf Nothing (Just 0) Nothing (Just 80) (Just 0)

f :: Conf -> [String] -> Maybe Conf
f conf [] = Just conf
f (Conf _ b c d e) ("-rule":arg:rest) =f (Conf (readMaybe arg) b c d e) rest
f (Conf a _ c d e) ("-start":arg:rest) = f (Conf a (readMaybe arg) c d e) rest
f (Conf a b _ d e) ("-lines":arg:rest) =
    let newLines = fmap (\x -> x - 1) (readMaybe arg)
    in f (Conf a b newLines d e) rest
f (Conf a b c _ e) ("-window":arg:rest) = f (Conf a b c (readMaybe arg) e) rest
f (Conf a b c d _) ("-move":arg:rest) = f (Conf a b c d (readMaybe arg)) rest
f _ _ = Nothing

parseOptions :: [String] -> Maybe Conf
parseOptions argv = f defaultConf argv

checkArgs :: Conf -> Maybe Conf
checkArgs (Conf (Just rule) start line window move)
    | rule `elem` [30, 90, 110] =
        Just (Conf (Just rule) start line window move)
    | otherwise = Nothing
checkArgs _ = Nothing

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

applyRule :: Conf -> [Bool] -> [Bool]
applyRule (Conf (Just 30) _ _ _ _) xs = True : rule30 xs ++ [True]
applyRule (Conf (Just 90) _ _ _ _) xs = True : rule90 xs ++ [True]
applyRule (Conf (Just 110) _ _ _ _) xs = True : rule110 xs ++ [True]
applyRule _ xs = xs

boolToChar :: Bool -> Char
boolToChar True = '*'
boolToChar False = ' '

display :: Conf -> [Char] -> IO ()
display _ [] = pure ()
display (Conf _ _ _ (Just window) (Just move)) xs =
    let len = length xs
        halfWindow = window `div` 2
        start = max 0 (len `div` 2 - halfWindow + move)
    in putStrLn $ replicate (halfWindow - len `div` 2 + move) ' '
        ++ take window (drop start xs)
display _ _ = pure ()

applyRuleNTimes :: Conf -> [Bool] -> IO ()
applyRuleNTimes conf@(Conf _ (Just start) Nothing _ _) xs
    | start > 0 = applyRuleNTimes (Conf (
            Just 30) (Just (start-1))
            Nothing (Just 80) (Just 0)) (applyRule conf xs)
    | otherwise = display conf (map boolToChar xs) >>= \_ ->
        applyRuleNTimes conf (applyRule conf xs)

applyRuleNTimes conf@(Conf _ (Just start) (Just 0) _ _) xs
    | start > 0 = return ()
    | otherwise = display conf (map boolToChar xs)

applyRuleNTimes conf@(Conf a (Just start) (Just n) d e) xs
    | start > 0 = applyRuleNTimes (
        Conf a (Just (start-1)) (Just n) d e) (applyRule conf xs)
    | otherwise = display conf (map boolToChar xs) >>= \_ ->
        applyRuleNTimes (
            Conf a (Just start) (Just (n-1)) d e) (applyRule conf xs)
applyRuleNTimes _ _ = pure ()
