module Data.Automata.NFA
    ( NFA
    , NFAInvalid(..)
    , getStates
    , getSymbols
    , getStartState
    , getAcceptingStates
    , getTransitionTable
    , getNowStates
    , newNFA
    , delta
    , deltaHat
    , isAccepted
    ) where

import Data.Automata
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Hashable
import Data.Either
import Data.Maybe
import Data.Monoid

data NFA state symbol = NFA
  { getStates :: S.HashSet state
  , getSymbols :: S.HashSet symbol
  , getStartState :: state
  , getAcceptingStates :: S.HashSet state
  , getTransitionTable :: M.HashMap (state, symbol) (S.HashSet state)
  , getNowStates :: S.HashSet state
  } deriving (Show, Eq)

instance Automata NFA where
  isAccepted (NFA _ _ _ acceptingStates _ nowStates) = 
    any (`S.member` acceptingStates) nowStates
  delta (NFA states symbols startState 
         acceptingStates transitionTable nowStates) 
        symbol =
    let
      newStates = 
        S.unions $
        map (fromMaybe S.empty . (\x -> M.lookup (x, symbol) transitionTable))
            (S.toList nowStates)
    in
      NFA states
          symbols
          startState
          acceptingStates
          transitionTable
          newStates


-------------------------------------------------------------------------------
-- * Construction

data NFAInvalid =
    NFAEmptyStates
  | NFAEmptySymbols
  | NFAInvalidStartState
  | NFAInvalidAcceptingStates
  | NFAInvalidInputState
  | NFAInvalidInputSymbol
  | NFAInvalidOutputState
  | NFANotAValidFunction
  deriving (Eq, Show)

validateStates :: (Hashable state, Eq state) => 
  [state] -> Either NFAInvalid (S.HashSet state)
validateStates [] = Left NFAEmptyStates
validateStates states = Right $ S.fromList states

validateSymbols :: (Hashable symbol, Eq symbol) =>
  [symbol] -> Either NFAInvalid (S.HashSet symbol)
validateSymbols [] = Left NFAEmptySymbols
validateSymbols symbols = Right $ S.fromList symbols

validateStartState :: (Hashable state, Eq state) =>
  S.HashSet state -> state -> Either NFAInvalid state
validateStartState stateList startState =
  if S.member startState stateList
    then Right startState
    else Left NFAInvalidStartState

validateAcceptingStates :: (Hashable state, Eq state) =>
  S.HashSet state -> [state] -> Either NFAInvalid (S.HashSet state)
validateAcceptingStates stateList acceptingStates =
  if all (`S.member` stateList) acceptingStates
    then Right $ S.fromList acceptingStates
    else Left NFAInvalidAcceptingStates

validateTableIO :: (Hashable state, Hashable symbol,
                    Eq state, Eq symbol) =>
     S.HashSet state -> S.HashSet symbol -> [((state, symbol), [state])]
  -> Maybe NFAInvalid
validateTableIO stateList symbolList transitionTable =
  case filter (not . and) $ 
       map (\((istate,isymbol),ostates) -> 
            [ S.member istate stateList
            , S.member isymbol symbolList
            , all (`S.member` stateList) ostates]) transitionTable of
    [] -> Nothing
    ([False,_,_]:_) -> Just NFAInvalidInputState
    ([_,False,_]:_) -> Just NFAInvalidInputSymbol
    ([_,_,False]:_) -> Just NFAInvalidOutputState

validateFunction :: (Hashable state, Hashable symbol,
                    Eq state, Eq symbol) =>
     [((state, symbol), [state])]
  -> Maybe NFAInvalid
validateFunction transitionTable =
  let
    domain = map fst transitionTable
    transitionNum = length transitionTable
    cardinalityOfDomain = S.size $ S.fromList domain
  in
    if transitionNum == cardinalityOfDomain 
      then Nothing
      else Just NFANotAValidFunction

validateTransitionTable :: (Hashable state, Hashable symbol,
                            Eq state, Eq symbol) =>
     S.HashSet state -> S.HashSet symbol -> [((state, symbol), [state])] 
  -> Either NFAInvalid (M.HashMap (state, symbol) (S.HashSet state))
validateTransitionTable stateList symbolList transitionTable =
  case getFirst $
    First (validateTableIO stateList symbolList transitionTable) <>
    First (validateFunction transitionTable) of
      Just err -> Left err
      Nothing -> Right $ M.map S.fromList $ M.fromList transitionTable

flattenTableSymbols :: (Hashable state, Hashable symbol,
                      Eq state, Eq symbol) =>
  [((state, [symbol]), [state])] -> [((state, symbol), [state])]
flattenTableSymbols [] = []
flattenTableSymbols (((istate, isymbols), ostate):table) =
  foldr (\x acc -> ((istate, x), ostate) : acc) (flattenTableSymbols table) isymbols

-- | Construct a NFA
newNFA :: (Hashable state, Hashable symbol, 
           Eq state, Eq symbol) =>
     [state]
  -> [symbol]
  -> state 
  -> [state]
  -> [((state, [symbol]), [state])]
  -> Either NFAInvalid (NFA state symbol)
newNFA states symbols startState acceptingStates transitionTable =
  let
    eitherStateList = validateStates states
    eitherSymbolList = validateSymbols symbols
  in
    case (eitherStateList, eitherSymbolList) of
      (Right stateList, Right symbolList) ->
        NFA stateList symbolList
          <$> validateStartState stateList startState
          <*> validateAcceptingStates stateList acceptingStates
          <*> validateTransitionTable stateList symbolList 
              (flattenTableSymbols transitionTable)
          <*> fmap S.singleton (validateStartState stateList startState)
      (Left err, _) -> Left err
      (_, Left err) -> Left err

