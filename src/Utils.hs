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
import Control.Monad.State

import ErrM

--
-- Data Types
--

type Var = String
type Boolean = Bool
type Loc = Integer

type LEnv' = Map Var Loc
type VEnv' vval = Map Loc vval
type FEnv' fval = Map Var fval

type ErrIO a = (ErrT IO) a

--
-- Functions
--

newLoc :: Map Loc a -> Loc
newLoc m 
    | Data.Map.null m    = 0
    | otherwise = (fst $ findMax m) + 1

keyFromValue :: Eq b => Map a b -> b -> a
keyFromValue m val = fst $ head $ toList $ Data.Map.filter (== val) m
