{-# LANGUAGE DeriveGeneric #-}
module Data.Automata.ConvertSpec (main, spec) where

import Test.Hspec
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import qualified Data.Automata.DFA as D
import qualified Data.Automata.NFA as N
import qualified Data.Automata.EpsNFA as E
import Data.Automata.Convert
import GHC.Generics
import Data.Hashable

data States = Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 
            | Q8 | Q9 | Q10 | Q11 | Q12 | Q13 | Q14
  deriving (Show, Eq, Generic, Ord, Enum)

instance Hashable States

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
  describe "converting" $ do
    it "convert DFA to EpsNFA" $ do
      let dfa = D.newDFA [Q0,Q1] 
                         "01"
                         Q0 
                         [Q1] 
                         [((Q0,"0"),Q0)
                         ,((Q0,"1"),Q1)
                         ,((Q1,"0"),Q0)
                         ,((Q1,"1"),Q1)] 
            :: Either D.DFAInvalid (D.DFA States Char)
      let epsnfa = E.newEpsNFA [Q0,Q1] 
                             "01"
                             Q0 
                             [Q1]
                             [((Q0,[Just '0']),[Q0])
                             ,((Q0,[Just '1']),[Q1])
                             ,((Q1,[Just '0']),[Q0])
                             ,((Q1,[Just '1']),[Q1])] 
            :: Either E.EpsNFAInvalid (E.EpsNFA States Char)
      case (dfa, epsnfa) of
        (Left err, _) -> expectationFailure $ show err
        (_, Left err) -> expectationFailure $ show err
        (Right d, Right e) -> convertDFAToEpsNFA d `shouldBe` e
    it "convert NFA to EpsNFA" $ do
      let nfa = N.newNFA [Q0,Q1] 
                         "01"
                         Q0 
                         [Q1] 
                         [((Q0,"0"),[Q0,Q1])
                         ,((Q0,"1"),[Q1])
                         ,((Q1,"0"),[Q0])
                         ,((Q1,"1"),[Q1])] 
            :: Either N.NFAInvalid (N.NFA States Char)
      let epsnfa = E.newEpsNFA [Q0,Q1] 
                             "01"
                             Q0 
                             [Q1]
                             [((Q0,[Just '0']),[Q0,Q1])
                             ,((Q0,[Just '1']),[Q1])
                             ,((Q1,[Just '0']),[Q0])
                             ,((Q1,[Just '1']),[Q1])] 
            :: Either E.EpsNFAInvalid (E.EpsNFA States Char)
      case (nfa, epsnfa) of
        (Left err, _) -> expectationFailure $ show err
        (_, Left err) -> expectationFailure $ show err
        (Right n, Right e) -> convertNFAToEpsNFA n `shouldBe` e
    it "convert EpsNFA to NFA" $ do
      let epsnfa = E.newEpsNFA (enumFromTo Q1 Q9)
                               "0" Q1 [Q8]
                               [((Q1, [Nothing]), [Q5])
                               ,((Q2, [Nothing]), [Q3,Q5])
                               ,((Q3, [Nothing]), [Q5])
                               ,((Q4, [Nothing]), [Q1])
                               ,((Q5, [Nothing]), [Q4,Q8])
                               ,((Q7, [Nothing]), [Q4])
                               ,((Q9, [Nothing]), [Q6])
                               ,((Q1, [Just '0']), [Q2])
                               ,((Q2, [Just '0']), [Q4])
                               ,((Q5, [Just '0']), [Q7,Q9])
                               ,((Q6, [Just '0']), [Q5])
                               ,((Q7, [Just '0']), [Q1])]
      let nfa = N.newNFA (enumFromTo Q1 Q9)
                         "0" Q1 [Q1,Q2,Q3,Q4,Q5,Q7,Q8]
                         [((Q1, "0"), [Q2,Q7,Q9])
                         ,((Q2, "0"), [Q2,Q4,Q7,Q9])
                         ,((Q3, "0"), [Q2,Q7,Q9])
                         ,((Q4, "0"), [Q2,Q7,Q9])
                         ,((Q5, "0"), [Q2,Q7,Q9])
                         ,((Q6, "0"), [Q5])
                         ,((Q7, "0"), [Q1,Q2,Q7,Q9])
                         ,((Q9, "0"), [Q5])]
      case (epsnfa, nfa) of
        (Left err, _) -> expectationFailure $ show err
        (_, Left err) -> expectationFailure $ show err
        (Right e, Right n) -> convertEpsNFAToNFA e `shouldBe` n
    it "convert NFA to DFA/simple" $ do
      let nfa = N.newNFA (enumFromTo Q0 Q2)
                         "01" Q0 [Q2]
                         [((Q0, "0"), [Q0,Q1])
                         ,((Q0, "1"), [Q0])
                         ,((Q1, "1"), [Q2])]
      let dfa = D.newDFA [S.fromList [Q0]
                         ,S.fromList [Q0,Q1]
                         ,S.fromList [Q0,Q2]]
                         "01" (S.fromList [Q0])
                         [S.fromList [Q0,Q2]]
                         [((S.fromList [Q0], "0"), S.fromList [Q0,Q1])
                         ,((S.fromList [Q0], "1"), S.fromList [Q0])
                         ,((S.fromList [Q0,Q1], "0"), S.fromList [Q0,Q1])
                         ,((S.fromList [Q0,Q1], "1"), S.fromList [Q0,Q2])
                         ,((S.fromList [Q0,Q2], "0"), S.fromList [Q0,Q1])
                         ,((S.fromList [Q0,Q2], "1"), S.fromList [Q0])]
      case (nfa, dfa) of
        (Left err, _) -> expectationFailure $ show err
        (_, Left err) -> expectationFailure $ show err
        (Right n, Right d) -> convertNFAToDFA n `shouldBe` d
    it "convert NFA to DFA/bad case" $ do
      let nfa = N.newNFA (enumFromTo Q0 Q3)
                         "01" Q0 [Q3]
                         [((Q0, "01"), [Q0])
                         ,((Q0, "1"), [Q1])
                         ,((Q1, "01"), [Q2])
                         ,((Q2, "01"), [Q3])]
      let dfa = D.newDFA [S.fromList [Q0]
                         ,S.fromList [Q0,Q1]
                         ,S.fromList [Q0,Q2]
                         ,S.fromList [Q0,Q3]
                         ,S.fromList [Q0,Q1,Q2]
                         ,S.fromList [Q0,Q2,Q3]
                         ,S.fromList [Q0,Q1,Q3]
                         ,S.fromList [Q0,Q1,Q2,Q3]]
                         "01" (S.fromList [Q0])
                         [S.fromList [Q0,Q3]
                         ,S.fromList [Q0,Q2,Q3]
                         ,S.fromList [Q0,Q1,Q3]
                         ,S.fromList [Q0,Q1,Q2,Q3]]
                         [((S.fromList [Q0],"0"),S.fromList [Q0])
                         ,((S.fromList [Q0,Q1],"0"),S.fromList [Q0,Q2])
                         ,((S.fromList [Q0,Q2],"0"),S.fromList [Q0,Q3])
                         ,((S.fromList [Q0,Q3],"0"),S.fromList [Q0])
                         ,((S.fromList [Q0,Q1,Q2],"0"),S.fromList [Q0,Q2,Q3])
                         ,((S.fromList [Q0,Q2,Q3],"0"),S.fromList [Q0,Q3])
                         ,((S.fromList [Q0,Q1,Q3],"0"),S.fromList [Q0,Q2])
                         ,((S.fromList [Q0,Q1,Q2,Q3],"0"),S.fromList [Q0,Q2,Q3])
                         ,((S.fromList [Q0],"1"),S.fromList [Q0,Q1])
                         ,((S.fromList [Q0,Q1],"1"),S.fromList [Q0,Q1,Q2])
                         ,((S.fromList [Q0,Q2],"1"),S.fromList [Q0,Q1,Q3])
                         ,((S.fromList [Q0,Q3],"1"),S.fromList [Q0,Q1])
                         ,((S.fromList [Q0,Q1,Q2],"1"),S.fromList [Q0,Q1,Q2,Q3])
                         ,((S.fromList [Q0,Q2,Q3],"1"),S.fromList [Q0,Q1,Q3])
                         ,((S.fromList [Q0,Q1,Q3],"1"),S.fromList [Q0,Q1,Q2])
                         ,((S.fromList [Q0,Q1,Q2,Q3],"1"),S.fromList [Q0,Q1,Q2,Q3])]
      case (nfa, dfa) of
        (Left err, _) -> expectationFailure $ show err
        (_, Left err) -> expectationFailure $ show err
        (Right n, Right d) -> convertNFAToDFA n `shouldBe` d
    it "convert NFA to DFA/text" $ do
      let nfa = N.newNFA (enumFromTo Q1 Q11)
                         "01" Q1 [Q5,Q8,Q11]
                         [((Q1,"0"),[Q1,Q2,Q9])
                         ,((Q1,"1"),[Q1,Q6])
                         ,((Q2,"1"),[Q3])
                         ,((Q3,"0"),[Q4])
                         ,((Q4,"1"),[Q5])
                         ,((Q6,"0"),[Q7])
                         ,((Q7,"1"),[Q8])
                         ,((Q9,"1"),[Q10])
                         ,((Q10,"1"),[Q11])]
      let a = S.fromList [Q1]
      let b = S.fromList [Q1,Q6]
      let c = S.fromList [Q1,Q2,Q9]
      let d = S.fromList [Q1,Q2,Q7,Q9]
      let e = S.fromList [Q1,Q3,Q6,Q10]
      let f = S.fromList [Q1,Q6,Q11]
      let g = S.fromList [Q1,Q2,Q4,Q7,Q9]
      let h = S.fromList [Q1,Q3,Q5,Q6,Q8,Q10]
      let i = S.fromList [Q1,Q3,Q6,Q8,Q10]
      let dfa = D.newDFA [a,b,c,d,e,f,g,h,i]
                         "01" a [f,h,i]
                         [((a,"0"),c)
                         ,((a,"1"),b)
                         ,((b,"0"),d)
                         ,((b,"1"),b)
                         ,((c,"0"),c)
                         ,((c,"1"),e)
                         ,((d,"0"),c)
                         ,((d,"1"),i)
                         ,((e,"0"),g)
                         ,((e,"1"),f)
                         ,((f,"0"),d)
                         ,((f,"1"),b)
                         ,((g,"0"),c)
                         ,((g,"1"),h)
                         ,((h,"0"),g)
                         ,((h,"1"),f)
                         ,((i,"0"),g)
                         ,((i,"1"),f)]
      case (nfa, dfa) of
        (Left err, _) -> expectationFailure $ show err
        (_, Left err) -> expectationFailure $ show err
        (Right n, Right d) -> convertNFAToDFA n `shouldBe` d
    it "convert EpsNFA to DFA/number" $ do
      let epsnfa = E.newEpsNFA [Q0,Q1,Q2,Q3,Q4,Q5] 
                       (['+','-','.']++['0'..'9'])
                       Q0
                       [Q5] 
                       [((Q0,[Nothing, Just '+', Just '-']),[Q1])
                       ,((Q1, map Just ['0'..'9']),[Q1,Q4])
                       ,((Q1, [Just '.']),[Q2])
                       ,((Q2, map Just ['0'..'9']),[Q3])
                       ,((Q3, map Just ['0'..'9']),[Q3])
                       ,((Q3, [Nothing]), [Q5])
                       ,((Q4, [Just '.']),[Q3])]
      let dfa = D.newDFA [S.fromList [Q0,Q1]
                       ,S.fromList [Q1]
                       ,S.fromList [Q1,Q4]
                       ,S.fromList [Q2]
                       ,S.fromList [Q2,Q3,Q5]
                       ,S.fromList [Q3,Q5]
                       ,S.empty]
                       (['+','-','.']++['0'..'9'])
                       (S.fromList [Q0,Q1])
                       [S.fromList [Q2,Q3,Q5]
                       ,S.fromList [Q3,Q5]]
                       [((S.fromList [Q0,Q1], "+-"), S.fromList [Q1])
                       ,((S.fromList [Q0,Q1], ['0'..'9']), S.fromList [Q1,Q4])
                       ,((S.fromList [Q0,Q1], "."), S.fromList [Q2])
                       ,((S.fromList [Q1], ['0'..'9']), S.fromList [Q1,Q4])
                       ,((S.fromList [Q1], "."), S.fromList [Q2])
                       ,((S.fromList [Q2], ['0'..'9']), S.fromList [Q3,Q5])
                       ,((S.fromList [Q1,Q4], ['0'..'9']), S.fromList [Q1,Q4])
                       ,((S.fromList [Q1,Q4], "."), S.fromList [Q2,Q3,Q5])
                       ,((S.fromList [Q2,Q3,Q5], ['0'..'9']), S.fromList [Q3,Q5])
                       ,((S.fromList [Q3,Q5], ['0'..'9']), S.fromList [Q3,Q5])
                       ,((S.fromList [Q1], "+-"), S.empty)
                       ,((S.fromList [Q1,Q4], "+-"), S.empty)
                       ,((S.fromList [Q2,Q3,Q5], "+-."), S.empty)
                       ,((S.fromList [Q2], "+-."), S.empty)
                       ,((S.fromList [Q3,Q5], "+-."), S.empty)
                       ,((S.empty, ['+','-','.']++['0'..'9']), S.empty)]
      case (epsnfa, dfa) of
        (Left err, _) -> expectationFailure $ show err
        (_, Left err) -> expectationFailure $ show err
        (Right e, Right d) -> convertEpsNFAToDFA e `shouldBe` d
    
    it "convert NFA to DFA/text" $ do
      let epsnfa = E.newEpsNFA (enumFromTo Q1 Q14)
                         "01" Q1 [Q5,Q8,Q11]
                         [((Q1,map Just "01"), [Q1])
                         ,((Q1,[Nothing]),[Q12,Q13,Q14])
                         ,((Q12,[Just '0']),[Q2])
                         ,((Q13,[Just '1']),[Q6])
                         ,((Q14,[Just '0']),[Q9])
                         ,((Q2,[Just '1']),[Q3])
                         ,((Q3,[Just '0']),[Q4])
                         ,((Q4,[Just '1']),[Q5])
                         ,((Q6,[Just '0']),[Q7])
                         ,((Q7,[Just '1']),[Q8])
                         ,((Q9,[Just '1']),[Q10])
                         ,((Q10,[Just '1']),[Q11])]
      let a = S.fromList [Q1,Q12,Q13,Q14]
      let b = S.fromList [Q1,Q12,Q13,Q14,Q6]
      let c = S.fromList [Q1,Q12,Q13,Q14,Q2,Q9]
      let d = S.fromList [Q1,Q12,Q13,Q14,Q2,Q7,Q9]
      let e = S.fromList [Q1,Q12,Q13,Q14,Q3,Q6,Q10]
      let f = S.fromList [Q1,Q12,Q13,Q14,Q6,Q11]
      let g = S.fromList [Q1,Q12,Q13,Q14,Q2,Q4,Q7,Q9]
      let h = S.fromList [Q1,Q12,Q13,Q14,Q3,Q5,Q6,Q8,Q10]
      let i = S.fromList [Q1,Q12,Q13,Q14,Q3,Q6,Q8,Q10]
      let dfa = D.newDFA [a,b,c,d,e,f,g,h,i]
                         "01" a [f,h,i]
                         [((a,"0"),c)
                         ,((a,"1"),b)
                         ,((b,"0"),d)
                         ,((b,"1"),b)
                         ,((c,"0"),c)
                         ,((c,"1"),e)
                         ,((d,"0"),c)
                         ,((d,"1"),i)
                         ,((e,"0"),g)
                         ,((e,"1"),f)
                         ,((f,"0"),d)
                         ,((f,"1"),b)
                         ,((g,"0"),c)
                         ,((g,"1"),h)
                         ,((h,"0"),g)
                         ,((h,"1"),f)
                         ,((i,"0"),g)
                         ,((i,"1"),f)]
      case (epsnfa, dfa) of
        (Left err, _) -> expectationFailure $ show err
        (_, Left err) -> expectationFailure $ show err
        (Right e, Right d) -> convertEpsNFAToDFA e `shouldBe` d