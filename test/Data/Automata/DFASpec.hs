{-# LANGUAGE DeriveGeneric #-}
module Data.Automata.DFASpec (main, spec) where

import Test.Hspec
import Data.Automata.DFA
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
    describe "can construct a DFA by newDFA" $ do
      let dfa = newDFA [Q0,Q1] 
                       "01"
                       Q0 
                       [Q1] 
                       [((Q0,"0"),Q0)
                       ,((Q0,"1"),Q1)
                       ,((Q1,"0"),Q0)
                       ,((Q1,"1"),Q1)] :: Either DFAInvalid (DFA States Char)
      it "states should match" $ 
        fmap getStates dfa `shouldBe` Right (S.fromList [Q0,Q1])
      it "symbols should match" $ 
        fmap getSymbols dfa `shouldBe` Right (S.fromList "01")
      it "startState should match" $ 
        fmap getStartState dfa `shouldBe` Right Q0
      it "acceptingStates should match" $ 
        fmap getAcceptingStates dfa `shouldBe` Right (S.fromList [Q1])
      it "transitionTable should match" $ 
        fmap getTransitionTable dfa `shouldBe` 
        Right (M.fromList [((Q0,'0'),Q0)
                          ,((Q0,'1'),Q1)
                          ,((Q1,'0'),Q0)
                          ,((Q1,'1'),Q1)])
      it "nowState should match" $
        fmap getNowState dfa `shouldBe` Right Q0
    describe "should handle multiple symbols in one definition" $ do
      let dfa = newDFA [Q0,Q1,Q2]
                       "012"
                       Q0
                       [Q2]
                       [((Q0,"012"),Q1)
                       ,((Q1,"012"),Q2)
                       ,((Q2,"012"),Q0)] :: Either DFAInvalid (DFA States Char)
      it "transitionTable should match" $ 
        fmap getTransitionTable dfa `shouldBe` 
        Right (M.fromList [((Q0,'0'),Q1)
                          ,((Q0,'1'),Q1)
                          ,((Q0,'2'),Q1)
                          ,((Q1,'0'),Q2)
                          ,((Q1,'1'),Q2)
                          ,((Q1,'2'),Q2)
                          ,((Q2,'0'),Q0)
                          ,((Q2,'1'),Q0)
                          ,((Q2,'2'),Q0)])
    describe "can detect errors during constructing" $ do
      it "empty acceptingStates are OK" $
        (newDFA [Q0] "1" Q0 [] [((Q0,"1"),Q0)] :: Either DFAInvalid (DFA States Char))
          `shouldSatisfy` isRight
      it "can detect an error in the states/empty" $
        (newDFA [] "1" Q0 [] [((Q0,"1"),Q0)] :: Either DFAInvalid (DFA States Char))
          `shouldBe` Left DFAEmptyStates
      it "can detect an error in the symbols/empty" $
        (newDFA [Q0] [] Q0 [] [((Q0,"1"),Q0)] :: Either DFAInvalid (DFA States Char))
          `shouldBe` Left DFAEmptySymbols
      it "can detect an error in the startState" $
        (newDFA [Q0] "1" Q1 [Q0] [((Q0,"1"),Q0)] :: Either DFAInvalid (DFA States Char))
          `shouldBe` Left DFAInvalidStartState
      it "can detect an error in the acceptingStates" $
        (newDFA [Q0] "1" Q0 [Q1] [((Q0,"1"),Q0)] :: Either DFAInvalid (DFA States Char))
          `shouldBe` Left DFAInvalidAcceptingStates
      it "can detect an error in the transitionTable/inputState" $
        (newDFA [Q0] "1" Q0 [Q0] [((Q1,"1"),Q0)] :: Either DFAInvalid (DFA States Char))
          `shouldBe` Left DFAInvalidInputState
      it "can detect an error in the transitionTable/inputSymbol" $
        (newDFA [Q0] "1" Q0 [Q0] [((Q0,"0"),Q0)] :: Either DFAInvalid (DFA States Char))
          `shouldBe` Left DFAInvalidInputSymbol
      it "can detect an error in the transitionTable/outputState" $
        (newDFA [Q0] "1" Q0 [Q0] [((Q0,"1"),Q1)] :: Either DFAInvalid (DFA States Char))
          `shouldBe` Left DFAInvalidOutputState
      it "can detect an error in the transitionTable/not a total function" $ do
        let dfa = newDFA [Q0,Q1] 
                         "01" 
                         Q0 
                         [Q1] 
                         [((Q0,"0"),Q0)
                         ,((Q0,"1"),Q1)] :: Either DFAInvalid (DFA States Char)
        dfa `shouldBe` Left DFANotATotalFunction
      it "can detect an error in the transitionTable/not a valid function" $ do
        let dfa = newDFA [Q0,Q1] 
                         "01" 
                         Q0 
                         [Q1] 
                         [((Q0,"0"),Q0)
                         ,((Q0,"1"),Q1)
                         ,((Q0,"0"),Q0)
                         ,((Q1,"0"),Q0)
                         ,((Q1,"1"),Q1)] :: Either DFAInvalid (DFA States Char)
        dfa `shouldBe` Left DFANotAValidFunction
  describe "Automata instance" $ do
    describe "isAccepted" $ do
      it "should not accept" $ do
        let dfa = newDFA [Q0,Q1] 
                         "01"
                         Q0 
                         [Q1] 
                         [((Q0,"0"),Q0)
                         ,((Q0,"1"),Q1)
                         ,((Q1,"0"),Q0)
                         ,((Q1,"1"),Q1)] :: Either DFAInvalid (DFA States Char)
        fmap isAccepted dfa `shouldBe` Right False
      it "should accept" $ do
        let dfa = newDFA [Q0,Q1] 
                         "01"
                         Q0 
                         [Q0] 
                         [((Q0,"0"),Q0)
                         ,((Q0,"1"),Q1)
                         ,((Q1,"0"),Q0)
                         ,((Q1,"1"),Q1)] :: Either DFAInvalid (DFA States Char)
        fmap isAccepted dfa `shouldBe` Right True
    describe "delta" $ do
      let dfaOrg = newDFA [Q0,Q1] 
                          "01"
                          Q0 
                          [Q1] 
                          [((Q0,"0"),Q0)
                          ,((Q0,"1"),Q1)
                          ,((Q1,"0"),Q0)
                          ,((Q1,"1"),Q1)] :: Either DFAInvalid (DFA States Char)
      let dfa1 = fmap (`delta` '0') dfaOrg
      let dfa2 = fmap (`delta` '1') dfaOrg
      it "states should not be modified" $ 
        fmap getStates dfa1 `shouldBe` Right (S.fromList [Q0,Q1])
      it "symbols should not be modified" $ 
        fmap getSymbols dfa1 `shouldBe` Right (S.fromList "01")
      it "startState should not be modified" $ 
        fmap getStartState dfa1 `shouldBe` Right Q0
      it "acceptingStates should not be modified" $ 
        fmap getAcceptingStates dfa1 `shouldBe` Right (S.fromList [Q1])
      it "transitionTable should not be modified" $ 
        fmap getTransitionTable dfa1 `shouldBe` 
          Right (M.fromList [((Q0,'0'),Q0)
                            ,((Q0,'1'),Q1)
                            ,((Q1,'0'),Q0)
                            ,((Q1,'1'),Q1)])
      it "nowState should match" $
        fmap getNowState dfa1 `shouldBe` Right Q0
      it "nowState should be modified" $
        fmap getNowState dfa2 `shouldBe` Right Q1
    describe "deltaHat" $ do
      let dfaOrg = newDFA [Q0,Q1] 
                          "01"
                          Q0 
                          [Q1] 
                          [((Q0,"0"),Q0)
                          ,((Q0,"1"),Q1)
                          ,((Q1,"0"),Q1)
                          ,((Q1,"1"),Q0)] :: Either DFAInvalid (DFA States Char)
      -- Odd number of 1's
      let dfa11 = fmap (`deltaHat` "0") dfaOrg
      let dfa21 = fmap (`deltaHat` "0110") dfaOrg
      let dfa31 = fmap (`deltaHat` "0000") dfaOrg
      let dfa41 = fmap (`deltaHat` "1111") dfaOrg
      let dfaQ1 = fmap (`deltaHat` "1") dfaOrg
      let dfa22 = fmap (`deltaHat` "1110") dfaOrg
      let dfa32 = fmap (`deltaHat` "1000") dfaOrg
      let dfa42 = fmap (`deltaHat` "1000") dfaOrg
      it "states should not be modified" $ 
        fmap getStates dfa11 `shouldBe` Right (S.fromList [Q0,Q1])
      it "symbols should not be modified" $ 
        fmap getSymbols dfa11 `shouldBe` Right (S.fromList "01")
      it "startState should not be modified" $ 
        fmap getStartState dfa11 `shouldBe` Right Q0
      it "acceptingStates should not be modified" $ 
        fmap getAcceptingStates dfa11 `shouldBe` Right (S.fromList [Q1])
      it "transitionTable should not be modified" $ 
        fmap getTransitionTable dfa11 `shouldBe` 
          Right (M.fromList [((Q0,'0'),Q0)
                            ,((Q0,'1'),Q1)
                            ,((Q1,'0'),Q1)
                            ,((Q1,'1'),Q0)])
      it "nowState should match/dfa11" $
        fmap getNowState dfa11 `shouldBe` Right Q0
      it "nowState should match/dfa21" $
        fmap getNowState dfa21 `shouldBe` Right Q0
      it "nowState should match/dfa31" $
        fmap getNowState dfa31 `shouldBe` Right Q0
      it "nowState should match/dfa41" $
        fmap getNowState dfa41 `shouldBe` Right Q0
      it "nowState should match/dfaQ1" $
        fmap getNowState dfaQ1 `shouldBe` Right Q1
      it "nowState should match/dfa22" $
        fmap getNowState dfa22 `shouldBe` Right Q1
      it "nowState should match/dfa32" $
        fmap getNowState dfa32 `shouldBe` Right Q1
      it "nowState should match/dfa42" $
        fmap getNowState dfa42 `shouldBe` Right Q1   
      
        
        
