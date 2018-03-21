module Data.Automata.EpsNFA
    ( EpsNFA
    , EpsNFAInvalid(..)
    , getStates
    , getSymbols
    , getStartState
    , getAcceptingStates
    , getTransitionTable
    , getNowStates
    , newEpsNFA
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

data EpsNFA state symbol = EpsNFA
  { getStates :: S.HashSet state
  , getSymbols :: S.HashSet symbol
  , getStartState :: state
  , getAcceptingStates :: S.HashSet state
  , getTransitionTable :: M.HashMap (state, Maybe symbol) (S.HashSet state)
  , getNowStates :: S.HashSet state
  } deriving (Show, Eq)

epsClosureFromTable :: (Hashable state, Hashable symbol,
                        Eq state, Eq symbol) =>
  M.HashMap (state, Maybe symbol) (S.HashSet state) -> state -> S.HashSet state
epsClosureFromTable transitionTable state =
  S.insert state $
  fromMaybe S.empty $
  M.lookup (state, Nothing) transitionTable

epsClosure :: (Hashable state, Hashable symbol,
               Eq state, Eq symbol) =>
  EpsNFA state symbol -> state -> S.HashSet state
epsClosure epsnfa = epsClosureFromTable (getTransitionTable epsnfa)

epsClosureEpsNFA :: (Hashable state, Hashable symbol,
                     Eq state, Eq symbol) =>
  EpsNFA state symbol -> EpsNFA state symbol
epsClosureEpsNFA (EpsNFA st sym stst accst tab nst) =
  EpsNFA st sym stst accst tab $
    S.unions $ map (epsClosureFromTable tab) (S.toList nst)

instance Automata EpsNFA where
  isAccepted (EpsNFA _ _ _ acceptingStates _ nowStates) = 
    any (`S.member` acceptingStates) nowStates
  delta (EpsNFA states symbols startState 
         acceptingStates transitionTable nowStates) 
        symbol =
    let
      newStates = 
        S.unions $
        map (fromMaybe S.empty . (\x -> M.lookup (x, Just symbol) transitionTable))
            (S.toList nowStates)
      newStatesWithEps =
        S.unions $
        newStates :
        map (fromMaybe S.empty . (\x -> M.lookup (x, Nothing) transitionTable))
            (S.toList newStates)
    in
      EpsNFA states
          symbols
          startState
          acceptingStates
          transitionTable
          newStatesWithEps


-------------------------------------------------------------------------------
-- * Construction

data EpsNFAInvalid =
    EpsNFAEmptyStates
  | EpsNFAEmptySymbols
  | EpsNFAInvalidStartState
  | EpsNFAInvalidAcceptingStates
  | EpsNFAInvalidInputState
  | EpsNFAInvalidInputSymbol
  | EpsNFAInvalidOutputState
  | EpsNFANotAValidFunction
  deriving (Eq, Show)

validateStates :: (Hashable state, Eq state) => 
  [state] -> Either EpsNFAInvalid (S.HashSet state)
validateStates [] = Left EpsNFAEmptyStates
validateStates states = Right $ S.fromList states

validateSymbols :: (Hashable symbol, Eq symbol) =>
  [symbol] -> Either EpsNFAInvalid (S.HashSet symbol)
validateSymbols [] = Left EpsNFAEmptySymbols
validateSymbols symbols = Right $ S.fromList symbols

validateStartState :: (Hashable state, Eq state) =>
  S.HashSet state -> state -> Either EpsNFAInvalid state
validateStartState stateList startState =
  if S.member startState stateList
    then Right startState
    else Left EpsNFAInvalidStartState

validateAcceptingStates :: (Hashable state, Eq state) =>
  S.HashSet state -> [state] -> Either EpsNFAInvalid (S.HashSet state)
validateAcceptingStates stateList acceptingStates =
  if all (`S.member` stateList) acceptingStates
    then Right $ S.fromList acceptingStates
    else Left EpsNFAInvalidAcceptingStates

validateTableIO :: (Hashable state, Hashable symbol,
                    Eq state, Eq symbol) =>
     S.HashSet state -> S.HashSet symbol -> [((state, Maybe symbol), [state])]
  -> Maybe EpsNFAInvalid
validateTableIO stateList symbolList transitionTable =
  case filter (not . and) $ 
       map (\((istate,isymbol),ostates) -> 
            [ S.member istate stateList
            , isNothing isymbol || S.member (fromJust isymbol) symbolList
            , all (`S.member` stateList) ostates]) transitionTable of
    [] -> Nothing
    ([False,_,_]:_) -> Just EpsNFAInvalidInputState
    ([_,False,_]:_) -> Just EpsNFAInvalidInputSymbol
    ([_,_,False]:_) -> Just EpsNFAInvalidOutputState

validateTransitionTable :: (Hashable state, Hashable symbol,
                            Eq state, Eq symbol) =>
     S.HashSet state -> S.HashSet symbol -> [((state, Maybe symbol), [state])] 
  -> Either EpsNFAInvalid (M.HashMap (state, Maybe symbol) (S.HashSet state))
validateTransitionTable stateList symbolList transitionTable =
  case validateTableIO stateList symbolList transitionTable of
      Just err -> Left err
      Nothing -> Right $ M.map S.fromList $ M.fromListWith (++) transitionTable

flattenTableSymbols :: (Hashable state, Hashable symbol,
                      Eq state, Eq symbol) =>
  [((state, [Maybe symbol]), [state])] -> [((state, Maybe symbol), [state])]
flattenTableSymbols [] = []
flattenTableSymbols (((istate, isymbols), ostate):table) =
  foldr (\x acc -> ((istate, x), ostate) : acc) (flattenTableSymbols table) isymbols

-- | Construct a EpsNFA
newEpsNFA :: (Hashable state, Hashable symbol, 
           Eq state, Eq symbol) =>
     [state]
  -> [symbol]
  -> state 
  -> [state]
  -> [((state, [Maybe symbol]), [state])]
  -> Either EpsNFAInvalid (EpsNFA state symbol)
newEpsNFA states symbols startState acceptingStates transitionTable =
  let
    eitherStateList = validateStates states
    eitherSymbolList = validateSymbols symbols
  in
    case (eitherStateList, eitherSymbolList) of
      (Right stateList, Right symbolList) ->
        fmap epsClosureEpsNFA $
        EpsNFA stateList symbolList
          <$> validateStartState stateList startState
          <*> validateAcceptingStates stateList acceptingStates
          <*> validateTransitionTable stateList symbolList 
              (flattenTableSymbols transitionTable)
          <*> fmap S.singleton (validateStartState stateList startState)
      (Left err, _) -> Left err
      (_, Left err) -> Left err

