import Lib

import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import Data.Function

main :: IO ()
main = hspec $ do

  describe "Account" $ do
    it "should work" $ do
        let account = EmptyAccount 
            in (account & (deposit 100) 
                    & (deposit 50) 
                    & (withdraw 25) 
                    & statement) `shouldBe` "Credit | Debit | Balance\n" ++
                                            " | 25 | 125\n" ++
                                            "50 |  | 150\n" ++
                                            "100 |  | 100"
        
    it "should do one deposit" $ do
        deposit 100 EmptyAccount `shouldBe` Account [Deposit 100]
    it "should do several deposits" $ do
        (EmptyAccount & (deposit 100) & (deposit 50)) `shouldBe` Account [Deposit 100, Deposit 50]

    it "should do one withdraw" $ do
        withdraw 100 EmptyAccount `shouldBe` Account [Withdraw 100]
    it "should do several withdraw" $ do
        (EmptyAccount & (withdraw 100) & (withdraw 50)) `shouldBe` Account [Withdraw 100, Withdraw 50]

    it "should print statement of empty account" $ do
        statement EmptyAccount `shouldBe` "Account Empty !"
    it "should print statement of account" $ do
        (statement (Account [Deposit 100, Deposit 50, Withdraw 50]))`shouldBe` "Credit | Debit | Balance\n" ++ 
                                                                               " | 50 | 100\n" ++
                                                                               "50 |  | 150\n" ++
                                                                               "100 |  | 100"
    
