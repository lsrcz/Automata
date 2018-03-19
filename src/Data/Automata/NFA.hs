module Data.Automata.NFA
    ( NFA
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
import Data.Maybe

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
  delta nfa@(NFA states symbols startState 
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

validateFunction :: (Hashable state, Hashable symbol, 
                     Eq state, Eq symbol) =>
  [((state, symbol), [state])] -> Bool
validateFunction transitionTable =
  let
    domain = map fst transitionTable
    sizeTableList = length transitionTable
    cardinalityOfDomain = S.size $ S.fromList domain
  in
    sizeTableList == cardinalityOfDomain

validate :: (Hashable state, Hashable symbol, 
             Eq state, Eq symbol) =>
  NFA state symbol -> Bool
validate (NFA states symbols startState
              acceptingStates transitionTable nowStates) =
     all (\(state, symbol) -> S.member state states && S.member symbol symbols)
         (M.keys transitionTable)
  && all (all (`S.member` states)) (M.elems transitionTable)
  && S.member startState states
  && all (`S.member` states) nowStates
  && all (`S.member` states) acceptingStates

-- | Construct a DFA
newNFA :: (Hashable state, Hashable symbol, 
           Eq state, Eq symbol) =>
     [state]
  -> [symbol]
  -> state 
  -> [state]
  -> [((state, symbol), [state])]
  -> Maybe (NFA state symbol)
newNFA states symbols startState acceptingStates transitionTable =
  let
    statesSet = S.fromList states
    symbolsSet = S.fromList symbols
    acceptingStateSet = S.fromList acceptingStates
    transitionTableMap = M.map S.fromList $ M.fromList transitionTable
    nfa = NFA statesSet symbolsSet startState 
              acceptingStateSet transitionTableMap (S.singleton startState)
  in 
    if validate nfa && validateFunction transitionTable
      then Just nfa
      else Nothing





