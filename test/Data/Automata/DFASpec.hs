module Data.Automata.DFASpec (main, spec) where

import Test.Hspec
import Data.Automata.DFA
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Maybe

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "construct" $ do
    describe "can construct a DFA by newDFA" $ do
      let dfa = newDFA [11,12] 
                       [21,22] 
                       11 
                       [12] 
                       [((11,21),11)
                       ,((11,22),12)
                       ,((12,21),11)
                       ,((12,22),12)] :: Maybe (DFA Int Int)
      it "states should match" $ 
        fmap getStates dfa `shouldBe` Just (S.fromList [11,12])
      it "symbols should match" $ 
        fmap getSymbols dfa `shouldBe` Just (S.fromList [21,22])
      it "startState should match" $ 
        fmap getStartState dfa `shouldBe` Just 11
      it "acceptingStates should match" $ 
        fmap getAcceptingStates dfa `shouldBe` Just (S.fromList [12])
      it "transitionTable should match" $ 
        fmap getTransitionTable dfa `shouldBe` 
          Just (M.fromList [((11,21),11)
                           ,((11,22),12)
                           ,((12,21),11)
                           ,((12,22),12)])
      it "nowState should match" $
        fmap getNowState dfa `shouldBe` Just 11
    describe "can detect errors during constructing" $ do
      it "empty symbols, acceptingStates are OK" $
        (newDFA [1] [2] 1 [] [((1,2),1)] :: Maybe (DFA Int Int))
          `shouldSatisfy` isJust
      it "can detect an error in the states/empty" $
        (newDFA [] [2] 1 [] [((1,2),1)] :: Maybe (DFA Int Int))
          `shouldBe` Nothing
      it "can detect an error in the startState" $
        (newDFA [1] [2] 2 [1] [((1,2),1)] :: Maybe (DFA Int Int))
          `shouldBe` Nothing
      it "can detect an error in the acceptingStates" $
        (newDFA [1] [2] 1 [2] [((1,2),1)] :: Maybe (DFA Int Int))
          `shouldBe` Nothing
      it "can detect an error in the transitionTable/inputState" $
        (newDFA [1] [2] 1 [1] [((2,2),1)] :: Maybe (DFA Int Int))
          `shouldBe` Nothing
      it "can detect an error in the transitionTable/inputSymbol" $
        (newDFA [1] [2] 1 [1] [((1,1),1)] :: Maybe (DFA Int Int))
          `shouldBe` Nothing
      it "can detect an error in the transitionTable/outputState" $
        (newDFA [1] [2] 1 [1] [((1,2),2)] :: Maybe (DFA Int Int))
          `shouldBe` Nothing
      it "can detect an error in the transitionTable/not a total function" $ do
        let dfa = newDFA [11,12] 
                         [21,22] 
                         11 
                         [12] 
                         [((11,21),11)
                         ,((11,22),12)] :: Maybe (DFA Int Int)
        dfa `shouldBe` Nothing
      it "can detect an error in the transitionTable/not a function" $ do
        let dfa = newDFA [11,12] 
                         [21,22] 
                         11 
                         [12] 
                         [((11,21),11)
                         ,((11,22),12)
                         ,((11,21),11)
                         ,((12,21),11)
                         ,((12,22),12)] :: Maybe (DFA Int Int)
        dfa `shouldBe` Nothing
  describe "Automata instance" $ do
    describe "isAccepted" $ do
      it "should not accept" $ do
        let dfa = newDFA [11,12] 
                         [21,22] 
                         11 
                         [12] 
                         [((11,21),11)
                         ,((11,22),12)
                         ,((12,21),11)
                         ,((12,22),12)] :: Maybe (DFA Int Int)
        fmap isAccepted dfa `shouldBe` Just False
      it "should accept" $ do
        let dfa = newDFA [11,12] 
                         [21,22] 
                         11 
                         [11] 
                         [((11,21),11)
                         ,((11,22),12)
                         ,((12,21),11)
                         ,((12,22),12)] :: Maybe (DFA Int Int)
        fmap isAccepted dfa `shouldBe` Just True
    describe "delta" $ do
      let dfaOrg = newDFA [11,12] 
                          [21,22] 
                          11 
                          [12] 
                          [((11,21),11)
                          ,((11,22),12)
                          ,((12,21),11)
                          ,((12,22),12)] :: Maybe (DFA Int Int)
      let dfa1 = fmap (`delta` 21) dfaOrg
      let dfa2 = fmap (`delta` 22) dfaOrg
      it "states should not be modified" $ 
        fmap getStates dfa1 `shouldBe` Just (S.fromList [11,12])
      it "symbols should not be modified" $ 
        fmap getSymbols dfa1 `shouldBe` Just (S.fromList [21,22])
      it "startState should not be modified" $ 
        fmap getStartState dfa1 `shouldBe` Just 11
      it "acceptingStates should not be modified" $ 
        fmap getAcceptingStates dfa1 `shouldBe` Just (S.fromList [12])
      it "transitionTable should not be modified" $ 
        fmap getTransitionTable dfa1 `shouldBe` 
          Just (M.fromList [((11,21),11)
                           ,((11,22),12)
                           ,((12,21),11)
                           ,((12,22),12)])
      it "nowState should match" $
        fmap getNowState dfa1 `shouldBe` Just 11
      it "nowState should be modified" $
        fmap getNowState dfa2 `shouldBe` Just 12
    describe "deltaHat" $ do
      let dfaOrg = newDFA [11,12] 
                          [21,22] 
                          11 
                          [12] 
                          [((11,21),11)
                          ,((11,22),12)
                          ,((12,21),12)
                          ,((12,22),11)] :: Maybe (DFA Int Int)
      -- Odd number of 12's
      let dfa11 = fmap (`deltaHat` [21]) dfaOrg
      let dfa21 = fmap (`deltaHat` [21, 22, 22, 21]) dfaOrg
      let dfa31 = fmap (`deltaHat` [21, 21, 21, 21]) dfaOrg
      let dfa41 = fmap (`deltaHat` [22, 22, 22, 22]) dfaOrg
      let dfa12 = fmap (`deltaHat` [22]) dfaOrg
      let dfa22 = fmap (`deltaHat` [22, 22, 22, 21]) dfaOrg
      let dfa32 = fmap (`deltaHat` [22, 21, 21, 21]) dfaOrg
      let dfa42 = fmap (`deltaHat` [21, 22, 22, 22]) dfaOrg
      it "states should not be modified" $ 
        fmap getStates dfa11 `shouldBe` Just (S.fromList [11,12])
      it "symbols should not be modified" $ 
        fmap getSymbols dfa11 `shouldBe` Just (S.fromList [21,22])
      it "startState should not be modified" $ 
        fmap getStartState dfa11 `shouldBe` Just 11
      it "acceptingStates should not be modified" $ 
        fmap getAcceptingStates dfa11 `shouldBe` Just (S.fromList [12])
      it "transitionTable should not be modified" $ 
        fmap getTransitionTable dfa11 `shouldBe` 
          Just (M.fromList [((11,21),11)
                           ,((11,22),12)
                           ,((12,21),12)
                           ,((12,22),11)])
      it "nowState should match/dfa11" $
        fmap getNowState dfa11 `shouldBe` Just 11
      it "nowState should match/dfa21" $
        fmap getNowState dfa21 `shouldBe` Just 11
      it "nowState should match/dfa31" $
        fmap getNowState dfa31 `shouldBe` Just 11
      it "nowState should match/dfa41" $
        fmap getNowState dfa41 `shouldBe` Just 11
      it "nowState should match/dfa12" $
        fmap getNowState dfa12 `shouldBe` Just 12
      it "nowState should match/dfa22" $
        fmap getNowState dfa22 `shouldBe` Just 12
      it "nowState should match/dfa32" $
        fmap getNowState dfa32 `shouldBe` Just 12
      it "nowState should match/dfa42" $
        fmap getNowState dfa42 `shouldBe` Just 12   
      
        
        
