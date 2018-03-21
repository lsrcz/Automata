module Data.Automata.DFA 
    ( DFA
    , DFAInvalid(..)
    , getStates
    , getSymbols
    , getStartState
    , getAcceptingStates
    , getTransitionTable
    , getNowState
    , newDFA
    , delta
    , deltaHat
    , isAccepted
    ) where

import Data.Automata
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Hashable
import Data.List(sort, groupBy)
import Data.Monoid

data DFA state symbol = DFA 
  { getStates :: S.HashSet state
  , getSymbols :: S.HashSet symbol
  , getStartState :: state
  , getAcceptingStates :: S.HashSet state
  , getTransitionTable :: M.HashMap (state, symbol) state
  , getNowState :: state
  } deriving (Show, Eq)

instance Automata DFA where
  isAccepted (DFA _ _ _ acceptingStates _ nowState) = 
    S.member nowState acceptingStates
  delta dfa@(DFA states symbols startState 
             acceptingStates transitionTable nowState) 
        symbol =
    case M.lookup (nowState, symbol) transitionTable of
      Nothing -> dfa
      Just state' -> 
        DFA states
            symbols
            startState
            acceptingStates
            transitionTable
            state'


-------------------------------------------------------------------------------
-- * Construction

data DFAInvalid =
    DFAEmptyStates
  | DFAEmptySymbols
  | DFAInvalidStartState
  | DFAInvalidAcceptingStates
  | DFAInvalidInputState
  | DFAInvalidInputSymbol
  | DFAInvalidOutputState
  | DFANotATotalFunction
  | DFANotAValidFunction
  deriving (Eq, Show)

validateStates :: (Hashable state, Eq state) => 
  [state] -> Either DFAInvalid (S.HashSet state)
validateStates [] = Left DFAEmptyStates
validateStates states = Right $ S.fromList states

validateSymbols :: (Hashable symbol, Eq symbol) =>
  [symbol] -> Either DFAInvalid (S.HashSet symbol)
validateSymbols [] = Left DFAEmptySymbols
validateSymbols symbols = Right $ S.fromList symbols

validateStartState :: (Hashable state, Eq state) =>
  S.HashSet state -> state -> Either DFAInvalid state
validateStartState stateList startState =
  if S.member startState stateList
    then Right startState
    else Left DFAInvalidStartState

validateAcceptingStates :: (Hashable state, Eq state) =>
  S.HashSet state -> [state] -> Either DFAInvalid (S.HashSet state)
validateAcceptingStates stateList acceptingStates =
  if all (`S.member` stateList) acceptingStates
    then Right $ S.fromList acceptingStates
    else Left DFAInvalidAcceptingStates
{-
validateTableInputStates :: (Hashable state, Hashable symbol,
                             Eq state, Eq symbol) =>
     S.HashSet state -> S.HashMap (state, symbol) state
  -> Maybe DFAInvalid
validateTableInputStates stateList transitionTable =
  if all (\(state, _) -> S.member state statesList)
         (M.keys transitionTable)
    then Nothing
    else Just DFAInvalidInputStates

validateTableInputSymbols :: (Hashable state, Hashable symbol,
                            Eq state, Eq symbol) =>
     S.HashSet symbol -> S.HashMap (state, symbol) state
  -> Maybe DFAInvalid
validateTableInputSymbols symbolList transitionTable =
  if all (\(_, symbol) -> S.member symbol symbolList)
         (M.keys transitionTable)
    then Nothing
    else Just DFAInvalidInputSymbols
-}
validateTableIO :: (Hashable state, Hashable symbol,
                    Eq state, Eq symbol) =>
     S.HashSet state -> S.HashSet symbol -> [((state, symbol), state)]
  -> Maybe DFAInvalid
validateTableIO stateList symbolList transitionTable =
  case filter (not . and) $ 
       map (\((istate,isymbol),ostate) -> 
            [ S.member istate stateList
            , S.member isymbol symbolList
            , S.member ostate stateList]) transitionTable of
    [] -> Nothing
    ([False,_,_]:_) -> Just DFAInvalidInputState
    ([_,False,_]:_) -> Just DFAInvalidInputSymbol
    ([_,_,False]:_) -> Just DFAInvalidOutputState

validateFunction :: (Hashable state, Hashable symbol,
                    Eq state, Eq symbol) =>
     S.HashSet state -> S.HashSet symbol -> [((state, symbol), state)]
  -> Maybe DFAInvalid
validateFunction stateList symbolList transitionTable =
  let
    stateNum = S.size stateList
    symbolNum = S.size symbolList
    domain = map fst transitionTable
    transitionNum = length transitionTable
    cardinalityOfDomain = S.size $ S.fromList domain
    cardinalityOfStates = S.size $ S.fromList $ map fst domain
    cardinalityOfSymbols = S.size $ S.fromList $ map snd domain
  in
    if transitionNum == cardinalityOfDomain 
      then 
        if stateNum == cardinalityOfStates && 
           symbolNum == cardinalityOfSymbols &&
           stateNum * symbolNum == cardinalityOfDomain
          then Nothing
          else Just DFANotATotalFunction
      else Just DFANotAValidFunction

validateTransitionTable :: (Hashable state, Hashable symbol,
                            Eq state, Eq symbol) =>
     S.HashSet state -> S.HashSet symbol -> [((state, symbol), state)] 
  -> Either DFAInvalid (M.HashMap (state, symbol) state)
validateTransitionTable stateList symbolList transitionTable =
  case getFirst $
    First (validateTableIO stateList symbolList transitionTable) <>
    First (validateFunction stateList symbolList transitionTable) of
      Just err -> Left err
      Nothing -> Right $ M.fromList transitionTable

flattenTableSymbols :: (Hashable state, Hashable symbol,
                      Eq state, Eq symbol) =>
  [((state, [symbol]), state)] -> [((state, symbol), state)]
flattenTableSymbols [] = []
flattenTableSymbols (((istate, isymbols), ostate):table) =
  foldr (\x acc -> ((istate, x), ostate) : acc) (flattenTableSymbols table) isymbols

-- | Construct a DFA
newDFA :: (Hashable state, Hashable symbol, 
           Eq state, Eq symbol) =>
     [state]
  -> [symbol]
  -> state 
  -> [state]
  -> [((state, [symbol]), state)]
  -> Either DFAInvalid (DFA state symbol)
newDFA states symbols startState acceptingStates transitionTable =
  let
    eitherStateList = validateStates states
    eitherSymbolList = validateSymbols symbols
  in
    case (eitherStateList, eitherSymbolList) of
      (Right stateList, Right symbolList) ->
        DFA stateList symbolList
          <$> validateStartState stateList startState
          <*> validateAcceptingStates stateList acceptingStates
          <*> validateTransitionTable stateList symbolList 
              (flattenTableSymbols transitionTable)
          <*> validateStartState stateList startState
      (Left err, _) -> Left err
      (_, Left err) -> Left err





