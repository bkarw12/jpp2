-- 
-- ########################################
-- Module for all compile-time error checks
-- ########################################
--

module Utils where

--
-- Imports
--

import Data.Map

--
-- Data Types
--

type Var = String
type Boolean = Bool
type Loc = Integer

--
-- Functions
--

newLoc :: Map Loc a -> Loc
newLoc m 
    | Data.Map.null m    = 0
    | otherwise = (fst $ findMax m) + 1

keyFromValue :: Eq b => Map a b -> b -> a
keyFromValue m val = fst $ head $ toList $ Data.Map.filter (== val) m
