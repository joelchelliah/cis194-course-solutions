{-# OPTIONS_GHC -Wall             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import           Employee
import           Data.Tree -- Exercise 2
import           Data.List -- Exercise 5


------------------- Exercise 1 -------------------

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp : emps) (fun + empFun emp)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL emps1 fun1) (GL emps2 fun2) = GL (emps1 ++ emps2) $ fun1 + fun2

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max


------------------- Exercise 2 -------------------

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node root forest) = f root $ treeFold f <$> forest


------------------- Exercise 3 -------------------

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss glPairs = (boss `addTo` noSubBossList, subBossList)
  where addTo e (GL es fun) = GL (e:es) (fun + empFun e)
        noSubBossList       = mconcat $ snd <$> glPairs
        subBossList         = mconcat $ fst <$> glPairs


------------------- Exercise 4 -------------------

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel


------------------- Exercise 5 -------------------

main :: IO ()
main = do
  content <- readFile "company.txt"

  let (GL emps fun) = maxFun $ read content

  mapM_ putStr [ "Total fun: ", show fun, "\n" ]
  mapM_ putStrLn . sort $ empName <$> emps
