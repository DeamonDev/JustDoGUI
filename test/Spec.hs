{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Test.QuickCheck
import TodoItem
import Control.Lens
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Gen

genPos :: Gen Int
genPos = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 0)

genListOfPos :: Gen [Int]
genListOfPos = listOf genPos

instance Arbitrary TodoItem where
  arbitrary = do
    Positive x <- arbitrary
    y <- arbitrary
    TodoItem x y <$> arbitrary

isAllDigit :: String -> Bool
isAllDigit s = True

prop_reverse :: [Int] -> Bool
prop_reverse xs = xs == xs

prop_markAsDone :: TodoItem -> Bool
prop_markAsDone todo
  | todo ^. done = todo ^. done == markAsDone todo ^. done
  | not (todo ^. done) = todo ^. done /= markAsDone todo ^. done

prop_markAsUndone :: TodoItem -> Bool 
prop_markAsUndone todo 
  | todo ^. done = todo ^. done /= markAsUndone todo ^. done
  | not (todo ^. done) = todo ^. done == markAsUndone todo ^. done

main :: IO ()
main = do
  putStrLn "We are starting tests..."
  quickCheck isAllDigit
  quickCheck prop_reverse
  quickCheck prop_markAsDone
  quickCheck prop_markAsUndone
  putStrLn "done!"
