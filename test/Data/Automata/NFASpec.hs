module Data.Automata.NFASpec (main, spec) where

import Test.Hspec
import Data.Automata.NFA
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Maybe

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "construct" $ do
    describe "can construct a NFA by newNFA" $ do
      let nfa = newNFA ["q0","q1","q2"] 
                       [0,1] 
                       "q0"
                       ["q2"] 
                       [(("q0",0),["q0","q1"])
                       ,(("q0",1),["q0"])
                       ,(("q1",1),["q2"])] :: Maybe (NFA String Int)
      it "states should match" $ 
        fmap getStates nfa `shouldBe` Just (S.fromList ["q0","q1","q2"])
      it "symbols should match" $ 
        fmap getSymbols nfa `shouldBe` Just (S.fromList [0,1])
      it "startState should match" $ 
        fmap getStartState nfa `shouldBe` Just "q0"
      it "acceptingStates should match" $ 
        fmap getAcceptingStates nfa `shouldBe` Just (S.fromList ["q2"])
      it "transitionTable should match" $ 
        fmap getTransitionTable nfa `shouldBe` 
          Just (M.fromList [(("q0",0),S.fromList ["q0","q1"])
                           ,(("q0",1),S.fromList ["q0"])
                           ,(("q1",1),S.fromList ["q2"])])
      it "nowStates should match" $
        fmap getNowStates nfa `shouldBe` Just (S.fromList ["q0"])
    describe "can detect errors during constructing" $ do
      it "empty symbols, acceptingStates and transitionTable are OK" $
        (newNFA [1] [2] 1 [] [] :: Maybe (NFA Int Int))
          `shouldSatisfy` isJust
      it "can detect an error in the states/empty" $
        (newNFA [] [2] 1 [] [((1,2),[1])] :: Maybe (NFA Int Int))
          `shouldBe` Nothing
      it "can detect an error in the startState" $
        (newNFA [1] [2] 2 [1] [((1,2),[1])] :: Maybe (NFA Int Int))
          `shouldBe` Nothing
      it "can detect an error in the acceptingStates" $
        (newNFA [1] [2] 1 [2] [((1,2),[1])] :: Maybe (NFA Int Int))
          `shouldBe` Nothing
      it "can detect an error in the transitionTable/inputState" $
        (newNFA [1] [2] 1 [1] [((2,2),[1])] :: Maybe (NFA Int Int))
          `shouldBe` Nothing
      it "can detect an error in the transitionTable/inputSymbol" $
        (newNFA [1] [2] 1 [1] [((1,1),[1])] :: Maybe (NFA Int Int))
          `shouldBe` Nothing
      it "can detect an error in the transitionTable/outputState" $
        (newNFA [1] [2] 1 [1] [((1,2),[2])] :: Maybe (NFA Int Int))
          `shouldBe` Nothing
      it "can detect an error in the transitionTable/not a function" $ do
        let nfa = newNFA [11,12] 
                         [21,22] 
                         11 
                         [12] 
                         [((11,21),[11])
                         ,((11,22),[12])
                         ,((11,21),[11])
                         ,((12,21),[11])
                         ,((12,22),[12])] :: Maybe (NFA Int Int)
        nfa `shouldBe` Nothing

  describe "Automata instance" $ do
    describe "isAccepted" $ do
      it "should not accept" $ do
        let nfa = newNFA [11,12] 
                         [21,22] 
                         11 
                         [12] 
                         [((11,21),[11])
                         ,((11,22),[12])
                         ,((12,21),[11])
                         ,((12,22),[12])] :: Maybe (NFA Int Int)
        fmap isAccepted nfa `shouldBe` Just False
      it "should accept" $ do
        let nfa = newNFA [11,12] 
                         [21,22] 
                         11 
                         [11] 
                         [((11,21),[11])
                         ,((11,22),[12])
                         ,((12,21),[11])
                         ,((12,22),[12])] :: Maybe (NFA Int Int)
        fmap isAccepted nfa `shouldBe` Just True
    describe "delta" $ do
      let nfaOrg = newNFA ["q0","q1","q2"] 
                          [0,1] 
                          "q0"
                          ["q2"] 
                          [(("q0",0),["q0","q1"])
                          ,(("q0",1),["q0"])
                          ,(("q1",1),["q2"])] :: Maybe (NFA String Int)
      let nfa1 = fmap (`delta` 0) nfaOrg
      let nfa2 = fmap (`delta` 1) nfaOrg
      let nfa3 = fmap (`delta` 2) nfaOrg
      it "states should not be modified" $ 
        fmap getStates nfa1 `shouldBe` Just (S.fromList ["q0","q1","q2"])
      it "symbols should not be modified" $ 
        fmap getSymbols nfa1 `shouldBe` Just (S.fromList [0,1])
      it "startState should not be modified" $ 
        fmap getStartState nfa1 `shouldBe` Just "q0"
      it "acceptingStates should not be modified" $ 
        fmap getAcceptingStates nfa1 `shouldBe` Just (S.fromList ["q2"])
      it "transitionTable should not be modified" $ 
        fmap getTransitionTable nfa1 `shouldBe` 
          Just (M.fromList [(("q0",0),S.fromList ["q0","q1"])
                           ,(("q0",1),S.fromList ["q0"])
                           ,(("q1",1),S.fromList ["q2"])])
      it "nowState should match/nfa1" $
        fmap getNowStates nfa1 `shouldBe` Just (S.fromList ["q0","q1"])
      it "nowState should match/nfa2" $
        fmap getNowStates nfa2 `shouldBe` Just (S.fromList ["q0"])
      it "nowState should match/nfa3" $
        fmap getNowStates nfa3 `shouldBe` Just (S.fromList [])
    describe "deltaHat" $ do
      let nfaOrg = newNFA ["q0","q1","q2"] 
                          [0,1] 
                          "q0"
                          ["q2"] 
                          [(("q0",0),["q0","q1"])
                          ,(("q0",1),["q0"])
                          ,(("q1",1),["q2"])] :: Maybe (NFA String Int)
      -- end in 01
      let nfa11 = fmap (`deltaHat` [0]) nfaOrg
      let nfa21 = fmap (`deltaHat` [0, 1, 1, 0]) nfaOrg
      let nfa31 = fmap (`deltaHat` [1, 0, 0, 0]) nfaOrg
      let nfa41 = fmap (`deltaHat` [0, 0, 1, 1]) nfaOrg
      let nfa12 = fmap (`deltaHat` [0, 1]) nfaOrg
      let nfa22 = fmap (`deltaHat` [0, 1, 0, 1]) nfaOrg
      let nfa32 = fmap (`deltaHat` [1, 0, 0, 1]) nfaOrg
      let nfa42 = fmap (`deltaHat` [0, 0, 0, 1]) nfaOrg
      it "states should not be modified" $ 
        fmap getStates nfa21 `shouldBe` Just (S.fromList ["q0","q1","q2"])
      it "symbols should not be modified" $ 
        fmap getSymbols nfa21 `shouldBe` Just (S.fromList [0,1])
      it "startState should not be modified" $ 
        fmap getStartState nfa21 `shouldBe` Just "q0"
      it "acceptingStates should not be modified" $ 
        fmap getAcceptingStates nfa21 `shouldBe` Just (S.fromList ["q2"])
      it "transitionTable should not be modified" $ 
        fmap getTransitionTable nfa21 `shouldBe` 
          Just (M.fromList [(("q0",0),S.fromList ["q0","q1"])
                           ,(("q0",1),S.fromList ["q0"])
                           ,(("q1",1),S.fromList ["q2"])])
      it "nowState should match/nfa11" $
        fmap getNowStates nfa11 `shouldBe` Just (S.fromList ["q0", "q1"])
      it "nowState should match/nfa21" $
        fmap getNowStates nfa21 `shouldBe` Just (S.fromList ["q0", "q1"])
      it "nowState should match/nfa31" $
        fmap getNowStates nfa31 `shouldBe` Just (S.fromList ["q0", "q1"])
      it "nowState should match/nfa41" $
        fmap getNowStates nfa41 `shouldBe` Just (S.fromList ["q0"])
      it "nowState should match/nfa12" $
        fmap getNowStates nfa12 `shouldBe` Just (S.fromList ["q0", "q2"])
      it "nowState should match/nfa22" $
        fmap getNowStates nfa22 `shouldBe` Just (S.fromList ["q0", "q2"])
      it "nowState should match/nfa32" $
        fmap getNowStates nfa32 `shouldBe` Just (S.fromList ["q0", "q2"])
      it "nowState should match/nfa42" $
        fmap getNowStates nfa42 `shouldBe` Just (S.fromList ["q0", "q2"])
      it "should not accept/nfa11" $
        fmap isAccepted nfa11 `shouldBe` Just False
      it "should not accept/nfa21" $
        fmap isAccepted nfa21 `shouldBe` Just False
      it "should not accept/nfa31" $
        fmap isAccepted nfa31 `shouldBe` Just False
      it "should not accept/nfa41" $
        fmap isAccepted nfa41 `shouldBe` Just False
      it "should accept/nfa12" $
        fmap isAccepted nfa12 `shouldBe` Just True
      it "should accept/nfa22" $
        fmap isAccepted nfa22 `shouldBe` Just True
      it "should accept/nfa32" $
        fmap isAccepted nfa32 `shouldBe` Just True
      it "should accept/nfa42" $
        fmap isAccepted nfa42 `shouldBe` Just True
    

