module Data.Automata.DFA 
    ( DFA
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

validateTotalFunction :: (Hashable state, Hashable symbol, 
                          Eq state, Eq symbol) =>
  S.HashSet state -> S.HashSet symbol -> [((state, symbol), state)] -> Bool
validateTotalFunction states symbols transitionTable =
  let
    stateNum = S.size states
    symbolNum = S.size symbols
    domain = map fst transitionTable
    sizeTableList = length transitionTable
    cardinalityOfDomain = S.size $ S.fromList domain
    cardinalityOfStates = S.size $ S.fromList $ map fst domain
    cardinalityOfSymbols = S.size $ S.fromList $ map snd domain
  in
    sizeTableList == cardinalityOfDomain &&
    stateNum == cardinalityOfStates && 
    symbolNum == cardinalityOfSymbols &&
    stateNum * symbolNum == cardinalityOfDomain

validate :: (Hashable state, Hashable symbol, 
             Eq state, Eq symbol) =>
  DFA state symbol -> Bool
validate (DFA states symbols startState
              acceptingStates transitionTable nowState) =
     all (\(state, symbol) -> S.member state states && S.member symbol symbols)
         (M.keys transitionTable)
  && all (`S.member` states) (M.elems transitionTable)
  && S.member startState states
  && S.member nowState states
  && all (`S.member` states) acceptingStates

-- | Construct a DFA
newDFA :: (Hashable state, Hashable symbol, 
           Eq state, Eq symbol) =>
     [state]
  -> [symbol]
  -> state 
  -> [state]
  -> [((state, symbol), state)]
  -> Maybe (DFA state symbol)
newDFA states symbols startState acceptingStates transitionTable =
  let
    statesSet = S.fromList states
    symbolsSet = S.fromList symbols
    acceptingStateSet = S.fromList acceptingStates
    transitionTableMap = M.fromList transitionTable
    dfa = DFA statesSet symbolsSet startState 
              acceptingStateSet transitionTableMap startState
  in
    if validate dfa && 
       validateTotalFunction statesSet symbolsSet transitionTable
      then Just dfa
      else Nothing





