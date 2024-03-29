------------------------------------------------------------------------------
--- Some tests for library RedBlackTree.
---
--- To run all tests automatically by the currycheck tool, use the command:
--- "curry check TestRedBlackTree"
--- 
--- @author Bernd Brassel, Michael Hanus
--- @version December 2018
------------------------------------------------------------------------------

import Data.List (nub)
import Test.Prop

import System.Random

import Data.RedBlackTree as RBT

intList2Tree = foldr update (RBT.empty (\ _ _ -> False) (==) (<))

rndTree n =
  getRandomSeed >>= return . nub . take n . (flip nextIntRange 100000) >>=
  \is -> return (intList2Tree is,is)

sorted [] = True
sorted [_] = True
sorted (x:y:xs) = x < y && sorted (y:xs)

rndDels n x = getRandomSeed >>= return . take n . (flip nextIntRange x)

deleteTest t _ [] = t
deleteTest t is (x:xs) = deleteTest (delete (is !! x) t) is xs

testIO m n =   
          rndTree m >>= \ (t,is) -> 
          rndDels n (length is) >>= \ ds ->
          let newt = deleteTest t is ds
           in return (sorted (toList newt))

-- Create tree with 1000 random entries, then randomly delete 100.
-- Test, if result is sorted.
testCreateRBTreeAndDeleteAndCheckSorted =
  (testIO 1000 100) `returns` True
