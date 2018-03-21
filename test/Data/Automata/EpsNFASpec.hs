{-# LANGUAGE DeriveGeneric #-}
module Data.Automata.EpsNFASpec (main, spec) where

import Test.Hspec
import Data.Automata.EpsNFA
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Either
import GHC.Generics
import Data.Hashable
import Data.List

data States = Q0 | Q1 | Q2 | Q3 | Q4 | Q5 deriving (Show, Eq, Generic, Ord)

instance Hashable States

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let epsnfa = newEpsNFA [Q0,Q1,Q2,Q3,Q4,Q5] 
                       (['+','-','.']++['0'..'9'])
                       Q0
                       [Q5] 
                       [((Q0,[Nothing, Just '+', Just '-']),[Q1])
                       ,((Q1, map Just ['0'..'9']),[Q1,Q4])
                       ,((Q1, [Just '.']),[Q2])
                       ,((Q2, map Just ['0'..'9']),[Q3])
                       ,((Q3, map Just ['0'..'9']),[Q3])
                       ,((Q3, [Nothing]), [Q5])
                       ,((Q4, [Just '.']),[Q3])] :: Either EpsNFAInvalid (EpsNFA States Char)
  describe "construct" $ do
    describe "can construct a EpsNFA by newEpsNFA" $ do
      it "states should match" $ 
        fmap getStates epsnfa `shouldBe` Right (S.fromList [Q0,Q1,Q2,Q3,Q4,Q5])
      it "symbols should match" $ 
        fmap getSymbols epsnfa `shouldBe` Right (S.fromList $ "+-."++['0'..'9'])
      it "startState should match" $ 
        fmap getStartState epsnfa `shouldBe` Right Q0
      it "acceptingStates should match" $ 
        fmap getAcceptingStates epsnfa `shouldBe` Right (S.fromList [Q5])
      it "transitionTable should match" $ 
        fmap getTransitionTable epsnfa `shouldBe` 
          Right (M.fromList [((Q0,Just '+'),S.fromList [Q1])
                            ,((Q0,Just '-'),S.fromList [Q1])
                            ,((Q0,Nothing),S.fromList [Q1])
                            ,((Q1,Just '0'),S.fromList [Q1,Q4])
                            ,((Q1,Just '1'),S.fromList [Q1,Q4])
                            ,((Q1,Just '2'),S.fromList [Q1,Q4])
                            ,((Q1,Just '3'),S.fromList [Q1,Q4])
                            ,((Q1,Just '4'),S.fromList [Q1,Q4])
                            ,((Q1,Just '5'),S.fromList [Q1,Q4])
                            ,((Q1,Just '6'),S.fromList [Q1,Q4])
                            ,((Q1,Just '7'),S.fromList [Q1,Q4])
                            ,((Q1,Just '8'),S.fromList [Q1,Q4])
                            ,((Q1,Just '9'),S.fromList [Q1,Q4])
                            ,((Q1,Just '.'),S.fromList [Q2])
                            ,((Q2,Just '0'),S.fromList [Q3])
                            ,((Q2,Just '1'),S.fromList [Q3])
                            ,((Q2,Just '2'),S.fromList [Q3])
                            ,((Q2,Just '3'),S.fromList [Q3])
                            ,((Q2,Just '4'),S.fromList [Q3])
                            ,((Q2,Just '5'),S.fromList [Q3])
                            ,((Q2,Just '6'),S.fromList [Q3])
                            ,((Q2,Just '7'),S.fromList [Q3])
                            ,((Q2,Just '8'),S.fromList [Q3])
                            ,((Q2,Just '9'),S.fromList [Q3])
                            ,((Q3,Just '0'),S.fromList [Q3])
                            ,((Q3,Just '1'),S.fromList [Q3])
                            ,((Q3,Just '2'),S.fromList [Q3])
                            ,((Q3,Just '3'),S.fromList [Q3])
                            ,((Q3,Just '4'),S.fromList [Q3])
                            ,((Q3,Just '5'),S.fromList [Q3])
                            ,((Q3,Just '6'),S.fromList [Q3])
                            ,((Q3,Just '7'),S.fromList [Q3])
                            ,((Q3,Just '8'),S.fromList [Q3])
                            ,((Q3,Just '9'),S.fromList [Q3])
                            ,((Q3,Nothing),S.fromList [Q5])
                            ,((Q4,Just '.'),S.fromList [Q3])])
      it "nowStates should be the epsilon closure of the start state" $
        fmap getNowStates epsnfa `shouldBe` Right (S.fromList [Q0,Q1])
    describe "can detect errors during constructing" $ do
      it "empty acceptingStates or transitionTable are OK" $
        (newEpsNFA [Q0] "0" Q0 [] [] :: Either EpsNFAInvalid (EpsNFA States Char))
          `shouldSatisfy` isRight
      it "can detect an error in the states/empty" $
        (newEpsNFA [] "0" Q0 [] [((Q0,[Just '0']),[Q0])] :: Either EpsNFAInvalid (EpsNFA States Char))
          `shouldBe` Left EpsNFAEmptyStates
      it "can detect an error in the symbols/empty" $
        (newEpsNFA [Q0] [] Q0 [] [((Q0,[Just '0']),[Q0])] :: Either EpsNFAInvalid (EpsNFA States Char))
          `shouldBe` Left EpsNFAEmptySymbols
      it "can detect an error in the startState" $
        (newEpsNFA [Q0] "0" Q1 [Q0] [((Q0,[Just '0']),[Q0])] :: Either EpsNFAInvalid (EpsNFA States Char))
          `shouldBe` Left EpsNFAInvalidStartState
      it "can detect an error in the acceptingStates" $
        (newEpsNFA [Q0] "0" Q0 [Q1] [((Q0,[Just '0']),[Q0])] :: Either EpsNFAInvalid (EpsNFA States Char))
          `shouldBe` Left EpsNFAInvalidAcceptingStates
      it "can detect an error in the transitionTable/inputState" $
        (newEpsNFA [Q0] "0" Q0 [Q0] [((Q1,[Just '0']),[Q0])] :: Either EpsNFAInvalid (EpsNFA States Char))
          `shouldBe` Left EpsNFAInvalidInputState
      it "can detect an error in the transitionTable/inputSymbol" $
        (newEpsNFA [Q0] "0" Q0 [Q0] [((Q0,[Just '1']),[Q0])] :: Either EpsNFAInvalid (EpsNFA States Char))
          `shouldBe` Left EpsNFAInvalidInputSymbol
      it "can detect an error in the transitionTable/outputState" $
        (newEpsNFA [Q0] "0" Q0 [Q0] [((Q0,[Just '0']),[Q1])] :: Either EpsNFAInvalid (EpsNFA States Char))
          `shouldBe` Left EpsNFAInvalidOutputState
      it ("duplicate entries or missing entries in the transitionTable of an EpsNFA" ++
         "is not an error") $ do
        let nfa = newEpsNFA [Q0,Q1] 
                         "01"
                         Q0 
                         [Q1] 
                         [((Q0,[Just '0']),[Q0])
                         ,((Q0,[Just '1']),[Q1])
                         ,((Q0,[Just '0']),[Q1])
                         ,((Q1,[Just '0']),[Q0])
                         ,((Q1,[Just '1']),[Q1])] :: Either EpsNFAInvalid (EpsNFA States Char)
        fmap getTransitionTable nfa `shouldBe` 
          Right (M.fromList [((Q0,Just '0'),S.fromList [Q0,Q1])
                            ,((Q0,Just '1'),S.fromList [Q1])
                            ,((Q1,Just '0'),S.fromList [Q0])
                            ,((Q1,Just '1'),S.fromList [Q1])])
  describe "Automata instance" $ do
    describe "isAccepted" $ do
      it "should not accept" $
        fmap isAccepted epsnfa `shouldBe` Right False
      it "should accept" $ do
        let epsnfa' = newEpsNFA [Q0,Q1,Q2,Q3,Q4,Q5] 
                               (['+','-','.']++['0'..'9'])
                               Q5
                               [Q5] 
                               [((Q0,[Nothing, Just '+', Just '-']),[Q1])
                               ,((Q1, map Just ['0'..'9']),[Q1,Q4])
                               ,((Q1, [Just '.']),[Q2])
                               ,((Q2, map Just ['0'..'9']),[Q3])
                               ,((Q3, map Just ['0'..'9']),[Q3])
                               ,((Q3, [Nothing]), [Q5])
                               ,((Q4, [Just '.']),[Q3])] :: Either EpsNFAInvalid (EpsNFA States Char)
        fmap isAccepted epsnfa' `shouldBe` Right True
    describe "delta" $ do
      let epsnfa1 = fmap (`delta` '+') epsnfa
      let epsnfa2 = fmap (`delta` '0') epsnfa
      let epsnfa3 = fmap (`delta` '.') epsnfa
      it "states should not be modified" $ 
        fmap getStates epsnfa1 `shouldBe` Right (S.fromList [Q0,Q1,Q2,Q3,Q4,Q5])
      it "symbols should not be modified" $ 
        fmap getSymbols epsnfa1 `shouldBe` Right (S.fromList $ "+-."++['0'..'9'])
      it "startState should not be modified" $ 
        fmap getStartState epsnfa1 `shouldBe` Right Q0
      it "acceptingStates should not be modified" $ 
        fmap getAcceptingStates epsnfa1 `shouldBe` Right (S.fromList [Q5])
      it "transitionTable should not be modified" $ 
        fmap getTransitionTable epsnfa1 `shouldBe` 
          Right (M.fromList [((Q0,Just '+'),S.fromList [Q1])
                            ,((Q0,Just '-'),S.fromList [Q1])
                            ,((Q0,Nothing),S.fromList [Q1])
                            ,((Q1,Just '0'),S.fromList [Q1,Q4])
                            ,((Q1,Just '1'),S.fromList [Q1,Q4])
                            ,((Q1,Just '2'),S.fromList [Q1,Q4])
                            ,((Q1,Just '3'),S.fromList [Q1,Q4])
                            ,((Q1,Just '4'),S.fromList [Q1,Q4])
                            ,((Q1,Just '5'),S.fromList [Q1,Q4])
                            ,((Q1,Just '6'),S.fromList [Q1,Q4])
                            ,((Q1,Just '7'),S.fromList [Q1,Q4])
                            ,((Q1,Just '8'),S.fromList [Q1,Q4])
                            ,((Q1,Just '9'),S.fromList [Q1,Q4])
                            ,((Q1,Just '.'),S.fromList [Q2])
                            ,((Q2,Just '0'),S.fromList [Q3])
                            ,((Q2,Just '1'),S.fromList [Q3])
                            ,((Q2,Just '2'),S.fromList [Q3])
                            ,((Q2,Just '3'),S.fromList [Q3])
                            ,((Q2,Just '4'),S.fromList [Q3])
                            ,((Q2,Just '5'),S.fromList [Q3])
                            ,((Q2,Just '6'),S.fromList [Q3])
                            ,((Q2,Just '7'),S.fromList [Q3])
                            ,((Q2,Just '8'),S.fromList [Q3])
                            ,((Q2,Just '9'),S.fromList [Q3])
                            ,((Q3,Just '0'),S.fromList [Q3])
                            ,((Q3,Just '1'),S.fromList [Q3])
                            ,((Q3,Just '2'),S.fromList [Q3])
                            ,((Q3,Just '3'),S.fromList [Q3])
                            ,((Q3,Just '4'),S.fromList [Q3])
                            ,((Q3,Just '5'),S.fromList [Q3])
                            ,((Q3,Just '6'),S.fromList [Q3])
                            ,((Q3,Just '7'),S.fromList [Q3])
                            ,((Q3,Just '8'),S.fromList [Q3])
                            ,((Q3,Just '9'),S.fromList [Q3])
                            ,((Q3,Nothing),S.fromList [Q5])
                            ,((Q4,Just '.'),S.fromList [Q3])])
      it "nowState should match/epsnfa1" $
        fmap getNowStates epsnfa1 `shouldBe` Right (S.fromList [Q1])
      it "nowState should match/epsnfa2" $
        fmap getNowStates epsnfa2 `shouldBe` Right (S.fromList [Q1,Q4])
      it "nowState should match/epsnfa3" $
        fmap getNowStates epsnfa3 `shouldBe` Right (S.fromList [Q2])
    describe "deltaHat" $ do
      -- decimal numbers
      let epsnfaac1 = fmap (`deltaHat` "0.") epsnfa
      let epsnfaac2 = fmap (`deltaHat` "+12.53") epsnfa
      let epsnfaac3 = fmap (`deltaHat` "-35.2") epsnfa
      let epsnfaac4 = fmap (`deltaHat` "+56784394883.647839") epsnfa
      let epsnfaac5 = fmap (`deltaHat` ".345") epsnfa
      let epsnfarj1 = fmap (`deltaHat` ".") epsnfa
      let epsnfarj2 = fmap (`deltaHat` "0") epsnfa
      let epsnfarj3 = fmap (`deltaHat` "+-1") epsnfa
      let epsnfarj4 = fmap (`deltaHat` "1.2.1") epsnfa
      let epsnfarj5 = fmap (`deltaHat` "1+1") epsnfa
      it "states should not be modified" $ 
        fmap getStates epsnfaac1 `shouldBe` Right (S.fromList [Q0,Q1,Q2,Q3,Q4,Q5])
      it "symbols should not be modified" $ 
        fmap getSymbols epsnfaac1 `shouldBe` Right (S.fromList $ "+-."++['0'..'9'])
      it "startState should not be modified" $ 
        fmap getStartState epsnfaac1 `shouldBe` Right Q0
      it "acceptingStates should not be modified" $ 
        fmap getAcceptingStates epsnfaac1 `shouldBe` Right (S.fromList [Q5])
      it "transitionTable should not be modified" $ 
        fmap getTransitionTable epsnfaac1 `shouldBe` 
          Right (M.fromList [((Q0,Just '+'),S.fromList [Q1])
                            ,((Q0,Just '-'),S.fromList [Q1])
                            ,((Q0,Nothing),S.fromList [Q1])
                            ,((Q1,Just '0'),S.fromList [Q1,Q4])
                            ,((Q1,Just '1'),S.fromList [Q1,Q4])
                            ,((Q1,Just '2'),S.fromList [Q1,Q4])
                            ,((Q1,Just '3'),S.fromList [Q1,Q4])
                            ,((Q1,Just '4'),S.fromList [Q1,Q4])
                            ,((Q1,Just '5'),S.fromList [Q1,Q4])
                            ,((Q1,Just '6'),S.fromList [Q1,Q4])
                            ,((Q1,Just '7'),S.fromList [Q1,Q4])
                            ,((Q1,Just '8'),S.fromList [Q1,Q4])
                            ,((Q1,Just '9'),S.fromList [Q1,Q4])
                            ,((Q1,Just '.'),S.fromList [Q2])
                            ,((Q2,Just '0'),S.fromList [Q3])
                            ,((Q2,Just '1'),S.fromList [Q3])
                            ,((Q2,Just '2'),S.fromList [Q3])
                            ,((Q2,Just '3'),S.fromList [Q3])
                            ,((Q2,Just '4'),S.fromList [Q3])
                            ,((Q2,Just '5'),S.fromList [Q3])
                            ,((Q2,Just '6'),S.fromList [Q3])
                            ,((Q2,Just '7'),S.fromList [Q3])
                            ,((Q2,Just '8'),S.fromList [Q3])
                            ,((Q2,Just '9'),S.fromList [Q3])
                            ,((Q3,Just '0'),S.fromList [Q3])
                            ,((Q3,Just '1'),S.fromList [Q3])
                            ,((Q3,Just '2'),S.fromList [Q3])
                            ,((Q3,Just '3'),S.fromList [Q3])
                            ,((Q3,Just '4'),S.fromList [Q3])
                            ,((Q3,Just '5'),S.fromList [Q3])
                            ,((Q3,Just '6'),S.fromList [Q3])
                            ,((Q3,Just '7'),S.fromList [Q3])
                            ,((Q3,Just '8'),S.fromList [Q3])
                            ,((Q3,Just '9'),S.fromList [Q3])
                            ,((Q3,Nothing),S.fromList [Q5])
                            ,((Q4,Just '.'),S.fromList [Q3])])
      it "nowState should match/0." $
        fmap getNowStates epsnfaac1 `shouldBe` Right (S.fromList [Q2,Q3,Q5])
      it "nowState should match/+12.53" $
        fmap getNowStates epsnfaac2 `shouldBe` Right (S.fromList [Q3,Q5])
      it "nowState should match/-35.2" $
        fmap getNowStates epsnfaac3 `shouldBe` Right (S.fromList [Q3,Q5])
      it "nowState should match/+56784394883.647839" $
        fmap getNowStates epsnfaac4 `shouldBe` Right (S.fromList [Q3,Q5])
      it "nowState should match/.345" $
        fmap getNowStates epsnfaac5 `shouldBe` Right (S.fromList [Q3,Q5])
      it "nowState should match/." $
        fmap getNowStates epsnfarj1 `shouldBe` Right (S.fromList [Q2])
      it "nowState should match/0" $
        fmap getNowStates epsnfarj2 `shouldBe` Right (S.fromList [Q1,Q4])
      it "nowState should match/+-1" $
        fmap getNowStates epsnfarj3 `shouldBe` Right (S.fromList [])
      it "nowState should match/1.2.1" $
        fmap getNowStates epsnfarj4 `shouldBe` Right (S.fromList [])
      it "nowState should match/1+1" $
        fmap getNowStates epsnfarj5 `shouldBe` Right (S.fromList [])
      it "should accept/0." $
        fmap isAccepted epsnfaac1 `shouldBe` Right True
      it "should accept/+12.53" $
        fmap isAccepted epsnfaac2 `shouldBe` Right True
      it "should accept/-35.2" $
        fmap isAccepted epsnfaac3 `shouldBe` Right True
      it "should accept/+56784394883.647839" $
        fmap isAccepted epsnfaac4 `shouldBe` Right True
      it "should accept/+.345" $
        fmap isAccepted epsnfaac5 `shouldBe` Right True
      it "should not accept/." $
        fmap isAccepted epsnfarj1 `shouldBe` Right False
      it "should not accept/0" $
        fmap isAccepted epsnfarj2 `shouldBe` Right False
      it "should not accept/+-1" $
        fmap isAccepted epsnfarj3 `shouldBe` Right False
      it "should not accept/1.2.1" $
        fmap isAccepted epsnfarj4 `shouldBe` Right False
      it "should not accept/1+1" $
        fmap isAccepted epsnfarj5 `shouldBe` Right False