module Lib where

import Data.List

data Account = EmptyAccount | Account [Transaction] deriving (Eq)
instance Show Account where
    show EmptyAccount = "[]"
    show (Account transactions) = show transactions

data Transaction = Deposit Int | Withdraw Int deriving (Eq)
instance Show Transaction where
    show (Deposit amount)= "deposit " ++ (show amount)
    show (Withdraw amount)= "withdraw " ++ (show amount)

deposit:: Int -> Account -> Account
deposit amount EmptyAccount = Account [(Deposit amount)]
deposit amount (Account transactions) = Account(transactions ++ [(Deposit amount)])

withdraw:: Int -> Account -> Account
withdraw amount EmptyAccount = Account [(Withdraw amount)]
withdraw amount (Account transactions) = Account(transactions ++ [(Withdraw amount)])

statement:: Account -> String
statement EmptyAccount = "Account Empty !"
statement (Account transactions) = intercalate ", " (map show transactions)

