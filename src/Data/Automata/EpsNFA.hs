module Data.Automata.EpsNFA
    ( EpsNFA(..)
    , EpsNFAInvalid(..)
    , newEpsNFA
    , delta
    , deltaHat
    , isAccepted
    , computeEpsClosureTable
    ) where

import Data.Automata
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Hashable
import Data.Either
import Data.Maybe
import Data.Monoid
import Control.Monad.ST
import Control.Monad
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

data EpsNFA state symbol = EpsNFA
  { getStates :: S.HashSet state
  , getSymbols :: S.HashSet symbol
  , getStartState :: state
  , getAcceptingStates :: S.HashSet state
  , getTransitionTable :: M.HashMap (state, Maybe symbol) (S.HashSet state)
  , getNowStates :: S.HashSet state
  , getEpsClosureTable :: M.HashMap state (S.HashSet state)
  } deriving (Show, Eq)

changeNowStates :: (Hashable state, Hashable symbol,
                    Eq state, Eq symbol) =>
  EpsNFA state symbol -> S.HashSet state -> EpsNFA state symbol
changeNowStates (EpsNFA st sym stst accst tab _ epstbl) newnst =
  EpsNFA st sym stst accst tab newnst epstbl


floyd :: Int -> U.Vector Bool -> U.Vector Bool
floyd len origGraph = runST $ do
  let 
    mread vec i j = M.unsafeRead vec (i * len + j)
    mwrite vec i j = M.unsafeWrite vec (i * len + j)
  graph <- U.thaw origGraph
  let 
    loop1 k
      | k == len = return ()
      | otherwise =
        let 
          loop2 i 
            | i == len = return ()
            | otherwise =
              let
                loop3 j
                  | j == len = return ()
                  | otherwise = do
                    xij <- mread graph i j
                    xik <- mread graph i k
                    xkj <- mread graph k j
                    mwrite graph i j (xij || (xik && xkj))
                    loop3 (j + 1)
              in loop3 0 >> loop2 (i + 1)
        in loop2 0 >> loop1 (k + 1)
  loop1 0
  U.unsafeFreeze graph


epsClosureTable :: (Hashable state, Hashable symbol,
                    Eq state, Eq symbol) =>
     EpsNFA state symbol
  -> M.HashMap state (S.HashSet state)
epsClosureTable epsnfa =
  let
    stateVec = V.fromList $ S.toList $ getStates epsnfa
    len = V.length stateVec
    graphVec = U.generate (len * len) f
    f i = 
      case divMod i len of
        (d,m) -> d == m || S.member (stateVec V.! m)
          (M.lookupDefault S.empty (stateVec V.! d, Nothing) $ 
          getTransitionTable epsnfa)
    accFunc acc idx x = if x then divMod idx len : acc else acc
    stateIdxList = U.ifoldl' accFunc [] $ floyd len graphVec
    statePairList = map (\(i,j) -> (stateVec V.! i, S.singleton $ stateVec V.! j)) stateIdxList
  in 
    M.fromListWith S.union statePairList

computeEpsClosureTable :: (Hashable state, Hashable symbol,
                           Eq state, Eq symbol) =>
  EpsNFA state symbol -> EpsNFA state symbol
computeEpsClosureTable epsnfa@(EpsNFA st sym stst accst tab nst _) =
  EpsNFA st sym stst accst tab nst $ epsClosureTable epsnfa

instance Automata EpsNFA where
  isAccepted (EpsNFA _ _ _ acceptingStates _ nowStates _) = 
    any (`S.member` acceptingStates) nowStates
  delta epsnfa@(EpsNFA _ _ _ _ transitionTable nowStates epsTable) 
        symbol =
    let
      newStates = 
        S.unions $
        map (fromMaybe S.empty . (\x -> M.lookup (x, Just symbol) transitionTable))
            (S.toList nowStates)
      newStatesWithEps =
        S.unions $
        map (fromMaybe S.empty . (`M.lookup` epsTable))
            (S.toList newStates)
    in
      changeNowStates epsnfa newStatesWithEps

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

setNowStates :: (Hashable state, Hashable symbol, 
                 Eq state, Eq symbol) =>
  EpsNFA state symbol -> EpsNFA state symbol
setNowStates epsnfa =
  changeNowStates epsnfa 
  (fromMaybe S.empty $ 
    M.lookup (getStartState epsnfa) 
    (getEpsClosureTable epsnfa))

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
        fmap (setNowStates . computeEpsClosureTable) $
        EpsNFA stateList symbolList
          <$> validateStartState stateList startState
          <*> validateAcceptingStates stateList acceptingStates
          <*> validateTransitionTable stateList symbolList 
              (flattenTableSymbols transitionTable)
          <*> Right S.empty
          <*> Right M.empty
      (Left err, _) -> Left err
      (_, Left err) -> Left err

