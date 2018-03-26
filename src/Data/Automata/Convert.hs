module Data.Automata.Convert 
  ( convertDFAToNFA
  , convertDFAToEpsNFA
  , convertNFAToDFA
  , convertNFAToEpsNFA
  , convertEpsNFAToDFA
  , convertEpsNFAToNFA
  ) where

import qualified Data.Automata.DFA as D
import qualified Data.Automata.NFA as N
import qualified Data.Automata.EpsNFA as E
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.List
import Data.Maybe
import Data.Hashable

-------------------------------------------------------------------------------
-- * Conversion

convertDFAToNFA :: (Hashable state, Hashable symbol, 
                    Eq state, Eq symbol) =>
  D.DFA state symbol -> N.NFA state symbol
convertDFAToNFA (D.DFA st sym stst accst tab nst) =
  N.NFA st sym stst accst 
        (M.fromList (map (\((ist,isym),ost) -> ((ist, isym),S.singleton ost))
                         (M.toList tab)))
        (S.singleton nst)

-- | Convert from DFA
convertDFAToEpsNFA :: (Hashable state, Hashable symbol, 
                       Eq state, Eq symbol) =>
  D.DFA state symbol -> E.EpsNFA state symbol
convertDFAToEpsNFA (D.DFA st sym stst accst tab nst) = 
  E.computeEpsClosureTable $
    E.EpsNFA st
             sym
             stst
             accst
             (M.fromList (map (\((ist,isym),ost) -> ((ist, Just isym),S.singleton ost))
                         (M.toList tab)))
             (S.singleton nst)
             M.empty

subsetConsNFA :: (Hashable state, Hashable symbol, 
                  Eq state, Eq symbol) =>
     N.NFA state symbol
  -> (S.HashSet (S.HashSet state)
     ,M.HashMap (S.HashSet state, symbol) (S.HashSet state))
subsetConsNFA nfa = 
  -- tab waitinglist alllist alltable
  go nfa [] S.empty M.empty
    where
      sym = N.getSymbols nfa
      symNum = S.size sym
      go nowNFA wList aList aTable =
        let
          nowStates = N.getNowStates nowNFA
          allNFAList = map (N.delta nowNFA) (S.toList sym)
          allTransList = zipWith3 (\x y z -> ((x,y),z))
            (replicate symNum nowStates) (S.toList sym) (map N.getNowStates allNFAList)
          newATable = aTable `M.union` M.fromList allTransList
          newAList = S.insert (N.getNowStates nowNFA) aList
          newWList = wList ++ filter (not . (`S.member` newAList) . N.getNowStates) allNFAList
        in
          case newWList of
            [] -> (newAList, newATable)
            (x:xs) -> go x xs newAList newATable

convertNFAToDFA :: (Hashable state, Hashable symbol, 
                       Eq state, Eq symbol) =>
  N.NFA state symbol -> D.DFA (S.HashSet state) symbol
convertNFAToDFA nfa@(N.NFA _ sym _ accst _ nst) = 
  let
    (newst, newTab) = subsetConsNFA nfa
  in
    D.DFA newst
          sym
          nst
          (S.filter (\s -> any (`S.member` s) accst) newst)
          newTab
          nst

-- | Convert from NFA
convertNFAToEpsNFA :: (Hashable state, Hashable symbol, 
                       Eq state, Eq symbol) =>
  N.NFA state symbol -> E.EpsNFA state symbol
convertNFAToEpsNFA (N.NFA st sym stst accst tab nst) = 
  E.computeEpsClosureTable $
    E.EpsNFA st
             sym
             stst
             accst
             (M.fromList (map (\((ist,isym),ost) -> ((ist, Just isym), ost))
                         (M.toList tab)))
             nst
             M.empty

subsetConsEpsNFA :: (Hashable state, Hashable symbol, 
                  Eq state, Eq symbol) =>
     E.EpsNFA state symbol
  -> (S.HashSet (S.HashSet state)
     ,M.HashMap (S.HashSet state, symbol) (S.HashSet state))
subsetConsEpsNFA epsnfa = 
  -- tab waitinglist alllist alltable
  go epsnfa [] S.empty M.empty
    where
      sym = E.getSymbols epsnfa
      symNum = S.size sym
      go nowEpsNFA wList aList aTable =
        let
          nowStates = E.getNowStates nowEpsNFA
          allEpsNFAList = map (E.delta nowEpsNFA) (S.toList sym)
          allTransList = zipWith3 (\x y z -> ((x,y),z))
            (replicate symNum nowStates) (S.toList sym) (map E.getNowStates allEpsNFAList)
          newATable = aTable `M.union` M.fromList allTransList
          newAList = S.insert (E.getNowStates nowEpsNFA) aList
          newWList = wList ++ filter (not . (`S.member` newAList) . E.getNowStates) allEpsNFAList
        in
          case newWList of
            [] -> (newAList, newATable)
            (x:xs) -> go x xs newAList newATable

convertEpsNFAToDFA :: (Hashable state, Hashable symbol, 
                       Eq state, Eq symbol) =>
  E.EpsNFA state symbol -> D.DFA (S.HashSet state) symbol
convertEpsNFAToDFA epsnfa@(E.EpsNFA _ sym _ accst _ nst _) = 
  let
    (newst, newTab) = subsetConsEpsNFA epsnfa
  in
    D.DFA newst
          sym
          nst
          (S.filter (\s -> any (`S.member` s) accst) newst)
          newTab
          nst

buildNFATransFromEpsTab :: (Hashable state, Hashable symbol, 
                            Eq state, Eq symbol) =>
     S.HashSet state -> S.HashSet symbol 
  -> M.HashMap (state, Maybe symbol) (S.HashSet state)
  -> M.HashMap state (S.HashSet state)
  -> M.HashMap (state, symbol) (S.HashSet state)
buildNFATransFromEpsTab states symbols transTab epsTab =
  foldl' 
    (\acc (ist, isym) -> 
      let
        epsClosure = fromJust $ M.lookup ist epsTab
        newTrans = S.unions $ map (\x -> fromMaybe S.empty $ M.lookup (x, Just isym) transTab) $ S.toList epsClosure
      in
        if S.null newTrans then acc else M.insert (ist, isym) newTrans acc
    ) 
  M.empty $
  (,) <$> S.toList states <*> S.toList symbols

buildNFAAccstFromEpsTab :: (Hashable state, Eq state) =>
  S.HashSet state -> M.HashMap state (S.HashSet state) -> S.HashSet state
buildNFAAccstFromEpsTab accst epstab =
  S.fromList $ M.keys $ M.filter (any (`S.member` accst) . S.toList) epstab

convertEpsNFAToNFA :: (Hashable state, Hashable symbol, 
                       Eq state, Eq symbol) =>
  E.EpsNFA state symbol -> N.NFA state symbol
convertEpsNFAToNFA (E.EpsNFA st sym stst accst tab _ epstab) = 
  N.NFA st
        sym
        stst
        (buildNFAAccstFromEpsTab accst epstab)
        (buildNFATransFromEpsTab st sym tab epstab)
        (S.singleton stst)
