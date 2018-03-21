{-# LANGUAGE DeriveGeneric #-}
module Data.Automata.NFASpec (main, spec) where

import Test.Hspec
import Data.Automata.NFA
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Either
import GHC.Generics
import Data.Hashable
import Data.List

data States = Q0 | Q1 | Q2 deriving (Show, Eq, Generic, Ord)

instance Hashable States

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "construct" $ do
    describe "can construct a NFA by newNFA" $ do
      let nfa = newNFA [Q0,Q1,Q2] 
                       "01"
                       Q0
                       [Q2] 
                       [((Q0,"0"),[Q0,Q1])
                       ,((Q0,"1"),[Q0])
                       ,((Q1,"1"),[Q2])] :: Either NFAInvalid (NFA States Char)
      it "states should match" $ 
        fmap getStates nfa `shouldBe` Right (S.fromList [Q0,Q1,Q2])
      it "symbols should match" $ 
        fmap getSymbols nfa `shouldBe` Right (S.fromList "01")
      it "startState should match" $ 
        fmap getStartState nfa `shouldBe` Right Q0
      it "acceptingStates should match" $ 
        fmap getAcceptingStates nfa `shouldBe` Right (S.fromList [Q2])
      it "transitionTable should match" $ 
        fmap getTransitionTable nfa `shouldBe` 
          Right (M.fromList [((Q0,'0'),S.fromList [Q0,Q1])
                            ,((Q0,'1'),S.fromList [Q0])
                            ,((Q1,'1'),S.fromList [Q2])])
      it "nowStates should match" $
        fmap getNowStates nfa `shouldBe` Right (S.fromList [Q0])
    describe "can detect errors during constructing" $ do
      it "empty acceptingStates or transitionTable are OK" $
        (newNFA [Q0] "0" Q0 [] [] :: Either NFAInvalid (NFA States Char))
          `shouldSatisfy` isRight
      it "can detect an error in the states/empty" $
        (newNFA [] "0" Q0 [] [((Q0,"0"),[Q0])] :: Either NFAInvalid (NFA States Char))
          `shouldBe` Left NFAEmptyStates
      it "can detect an error in the symbols/empty" $
        (newNFA [Q0] [] Q0 [] [((Q0,"0"),[Q0])] :: Either NFAInvalid (NFA States Char))
          `shouldBe` Left NFAEmptySymbols
      it "can detect an error in the startState" $
        (newNFA [Q0] "0" Q1 [Q0] [((Q0,"0"),[Q0])] :: Either NFAInvalid (NFA States Char))
          `shouldBe` Left NFAInvalidStartState
      it "can detect an error in the acceptingStates" $
        (newNFA [Q0] "0" Q0 [Q1] [((Q0,"0"),[Q0])] :: Either NFAInvalid (NFA States Char))
          `shouldBe` Left NFAInvalidAcceptingStates
      it "can detect an error in the transitionTable/inputState" $
        (newNFA [Q0] "0" Q0 [Q0] [((Q1,"0"),[Q0])] :: Either NFAInvalid (NFA States Char))
          `shouldBe` Left NFAInvalidInputState
      it "can detect an error in the transitionTable/inputSymbol" $
        (newNFA [Q0] "0" Q0 [Q0] [((Q0,"1"),[Q0])] :: Either NFAInvalid (NFA States Char))
          `shouldBe` Left NFAInvalidInputSymbol
      it "can detect an error in the transitionTable/outputState" $
        (newNFA [Q0] "0" Q0 [Q0] [((Q0,"0"),[Q1])] :: Either NFAInvalid (NFA States Char))
          `shouldBe` Left NFAInvalidOutputState
      it "can detect an error in the transitionTable/not a function" $ do
        let nfa = newNFA [Q0,Q1] 
                         "01"
                         Q0 
                         [Q1] 
                         [((Q0,"0"),[Q0])
                         ,((Q0,"1"),[Q1])
                         ,((Q0,"0"),[Q0])
                         ,((Q1,"0"),[Q0])
                         ,((Q1,"1"),[Q1])] :: Either NFAInvalid (NFA States Char)
        nfa `shouldBe` Left NFANotAValidFunction

  describe "Automata instance" $ do
    describe "isAccepted" $ do
      it "should not accept" $ do
        let nfa = newNFA [Q0,Q1] 
                         "01"
                         Q0 
                         [Q1] 
                         [((Q0,"0"),[Q0])
                         ,((Q0,"1"),[Q1])
                         ,((Q1,"0"),[Q0])
                         ,((Q1,"1"),[Q1])] :: Either NFAInvalid (NFA States Char)
        fmap isAccepted nfa `shouldBe` Right False
      it "should accept" $ do
        let nfa = newNFA [Q0,Q1] 
                         "01"
                         Q0 
                         [Q0] 
                         [((Q0,"0"),[Q0])
                         ,((Q0,"1"),[Q1])
                         ,((Q1,"0"),[Q0])
                         ,((Q1,"1"),[Q1])] :: Either NFAInvalid (NFA States Char)
        fmap isAccepted nfa `shouldBe` Right True
    describe "delta" $ do
      let nfaOrg = newNFA [Q0,Q1,Q2] 
                          "01"
                          Q0
                          [Q2] 
                          [((Q0,"0"),[Q0,Q1])
                          ,((Q0,"1"),[Q0])
                          ,((Q1,"1"),[Q2])] :: Either NFAInvalid (NFA States Char)
      let nfa1 = fmap (`delta` '0') nfaOrg
      let nfa2 = fmap (`delta` '1') nfaOrg
      let nfa3 = fmap (`delta` '2') nfaOrg
      it "states should not be modified" $ 
        fmap getStates nfa1 `shouldBe` Right (S.fromList [Q0,Q1,Q2])
      it "symbols should not be modified" $ 
        fmap getSymbols nfa1 `shouldBe` Right (S.fromList "01")
      it "startState should not be modified" $ 
        fmap getStartState nfa1 `shouldBe` Right Q0
      it "acceptingStates should not be modified" $ 
        fmap getAcceptingStates nfa1 `shouldBe` Right (S.fromList [Q2])
      it "transitionTable should not be modified" $ 
        fmap getTransitionTable nfa1 `shouldBe` 
          Right (M.fromList [((Q0,'0'),S.fromList [Q0,Q1])
                            ,((Q0,'1'),S.fromList [Q0])
                            ,((Q1,'1'),S.fromList [Q2])])
      it "nowState should match/nfa1" $
        fmap getNowStates nfa1 `shouldBe` Right (S.fromList [Q0,Q1])
      it "nowState should match/nfa2" $
        fmap getNowStates nfa2 `shouldBe` Right (S.fromList [Q0])
      it "nowState should match/nfa3" $
        fmap getNowStates nfa3 `shouldBe` Right (S.fromList [])
    describe "deltaHat" $ do
      let nfaOrg = newNFA [Q0,Q1,Q2] 
                          "01"
                          Q0
                          [Q2] 
                          [((Q0,"0"),[Q0,Q1])
                          ,((Q0,"1"),[Q0])
                          ,((Q1,"1"),[Q2])] :: Either NFAInvalid (NFA States Char)
      -- end in 01
      let nfa11 = fmap (`deltaHat` "0") nfaOrg
      let nfa21 = fmap (`deltaHat` "0110") nfaOrg
      let nfa31 = fmap (`deltaHat` "1000") nfaOrg
      let nfa41 = fmap (`deltaHat` "0011") nfaOrg
      let nfa12 = fmap (`deltaHat` "01") nfaOrg
      let nfa22 = fmap (`deltaHat` "0101") nfaOrg
      let nfa32 = fmap (`deltaHat` "1001") nfaOrg
      let nfa42 = fmap (`deltaHat` "0001") nfaOrg
      it "states should not be modified" $ 
        fmap getStates nfa21 `shouldBe` Right (S.fromList [Q0,Q1,Q2])
      it "symbols should not be modified" $ 
        fmap getSymbols nfa21 `shouldBe` Right (S.fromList "01")
      it "startState should not be modified" $ 
        fmap getStartState nfa21 `shouldBe` Right Q0
      it "acceptingStates should not be modified" $ 
        fmap getAcceptingStates nfa21 `shouldBe` Right (S.fromList [Q2])
      it "transitionTable should not be modified" $ 
        fmap getTransitionTable nfa21 `shouldBe` 
          Right (M.fromList [((Q0,'0'),S.fromList [Q0,Q1])
                            ,((Q0,'1'),S.fromList [Q0])
                            ,((Q1,'1'),S.fromList [Q2])])
      it "nowState should match/nfa11" $
        fmap getNowStates nfa11 `shouldBe` Right (S.fromList [Q0, Q1])
      it "nowState should match/nfa21" $
        fmap getNowStates nfa21 `shouldBe` Right (S.fromList [Q0, Q1])
      it "nowState should match/nfa31" $
        fmap getNowStates nfa31 `shouldBe` Right (S.fromList [Q0, Q1])
      it "nowState should match/nfa41" $
        fmap getNowStates nfa41 `shouldBe` Right (S.fromList [Q0])
      it "nowState should match/nfa12" $
        fmap getNowStates nfa12 `shouldBe` Right (S.fromList [Q0, Q2])
      it "nowState should match/nfa22" $
        fmap getNowStates nfa22 `shouldBe` Right (S.fromList [Q0, Q2])
      it "nowState should match/nfa32" $
        fmap getNowStates nfa32 `shouldBe` Right (S.fromList [Q0, Q2])
      it "nowState should match/nfa42" $
        fmap getNowStates nfa42 `shouldBe` Right (S.fromList [Q0, Q2])
      it "should not accept/nfa11" $
        fmap isAccepted nfa11 `shouldBe` Right False
      it "should not accept/nfa21" $
        fmap isAccepted nfa21 `shouldBe` Right False
      it "should not accept/nfa31" $
        fmap isAccepted nfa31 `shouldBe` Right False
      it "should not accept/nfa41" $
        fmap isAccepted nfa41 `shouldBe` Right False
      it "should accept/nfa12" $
        fmap isAccepted nfa12 `shouldBe` Right True
      it "should accept/nfa22" $
        fmap isAccepted nfa22 `shouldBe` Right True
      it "should accept/nfa32" $
        fmap isAccepted nfa32 `shouldBe` Right True
      it "should accept/nfa42" $
        fmap isAccepted nfa42 `shouldBe` Right True
    

