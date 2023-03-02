module Main where

import Control.Monad.Except (forM)
import Control.Monad (foldM)

import Test.Hspec ( hspec, describe, Spec )

import Spec.Tests
import Spec.IOTest ( spec )
import Data.Maybe (fromJust, isJust)
import Data.List (transpose, delete)

type Description = String

filterDependentTests :: Monad m => [[Maybe a]] -> m [Maybe [a]]
filterDependentTests exs = return (foldr (zipWith (\x xs -> (:) <$> x <*> xs)) (repeat (Just [])) exs)

runner :: Monad m
            => Description
            -> [[Maybe a]]
            -> ([a] -> m (Maybe b, Spec))
            -> m ([Maybe b], Spec)
runner descr exs spectest = do
  dependencyFullfilled <- filterDependentTests exs
  tested <- forM dependencyFullfilled $ \a -> case a of 
                                                Nothing -> return (Nothing, pure ())
                                                Just xs -> spectest xs
  sequenced <- foldM f ([], return ()) tested
  case sequenced of
    (bs, specs) -> return (bs, describe descr specs)
  where f (bs, specsequence) (b, spec) = return (b:bs, spec >> specsequence)


---------------------------- NOn linear runner ---------------------
{-
getEvalOrder :: Monad m => [(Maybe a, a -> [a])] -> m [Maybe a]
getEvalOrder exs = 
  let listsOfDeps = map (\(x, f) -> case x of                     -- listsOfDeps hat dependencies für Element exs_i an index i
                                            Nothing -> []
                                            Just v -> f v)
                        exs 
  in evalOrderWorker (map fst exs) listsOfDeps
                    
-- Bin mir nicht sicher, ob das so funktionieren kann. Wollte es so haben, dass die Liste deps wiederholt abgelaufen wird. 
-- Falls deps an einer Stelle i empty ist, dann hat vals and index i keine dependencies die nicht erfüllt wurden -> nächstes Element in Testreihenfolge order
--                                                                                                               -> vals_i muss aus allen deps gelöscht werden
evalOrderWorker :: Monad m => [Maybe a] -> [[a]] -> m [Maybe a]
evalOrderWorker vals deps order = return (f vals deps order)
  where f vs [] ord = ord
        f (v:vs) ([]:ds) ord = evalOrderWorker (delete v vals) (map (delete v) deps) (order ++ [v]) 
        f (v:vs) (d:ds) ord = f vs ds ord

  

runnerNonLinear :: Monad m 
                  => Description
                  -> [(Maybe a, a -> [a])]                     -- List of values and their dependencies
                  -> (a -> m (Maybe b, Spec))
                  -> m ([Maybe b], Spec)
runnerNonLinear descr exs spectest = do                      
  evaluationOrder <- getEvalOrder exs 
  tested <- forM evaluationOrder $ \a -> case a of 
                                                Nothing -> return (Nothing, pure ())
                                                Just x -> spectest x
  sequenced <- foldM f ([], return ()) tested
  case sequenced of
    (bs, specs) -> return (bs, describe descr specs)
  where f (bs, specsequence) (b, spec) = return (b:bs, spec >> specsequence)
                    
-}


fromTo :: Int -> Int -> [Int]
fromTo x y | x > y = []
           | otherwise = x : fromTo (x + 1) y





main :: IO ()
main = do
    let examples = map Just $ fromTo 1 10
    print examples
    (biggerThan5examples, biggerThan5spec) <- runner "Only examples bigger than 5" [examples] (\[ex] -> Spec.Tests.biggerThan5 ex)

    (evenExamples, evenSpec) <- runner "Only even examples" [examples] (\[ex] -> Spec.Tests.onlyEven ex)

    -- Result of second dependency not used:
    (evenAndBiggerThan5, dependentSpec) <- runner "Dependent on the first two tests: Bigger than 5 and even" [biggerThan5examples, evenExamples] (\[x1, _] -> Spec.Tests.onlyEvenBiggerThan5 x1)

    -- Result of second dependecy used: 
    (xs1, t1) <- runner "Numbers are not odd" [examples] (\[x] -> Spec.Tests.f1_modulo3 x)
    (xs2, t2) <- runner "These Numbers are also not odd" [examples] (\[x] -> Spec.Tests.f2_minus5 x)

    -- Use both dependencies: 
    (productsNotOdd, doubleDependentSpec) <- runner "f (f1 x) (f2 x) not odd" [xs1, xs2] (\[x1, x2] -> Spec.Tests.productNotOdd x1 x2)


    -----------------Monadische Werte-----------------
    
    (_, mv) <- runner "Reading in a file of numbers, are they bigger than these examples?" [examples] (\[x] -> Spec.IOTest.spec x)

    ----------------Nicht lineare Tests---------------

    (_, nonlin) <- runner "Does a number reach zero if you substract 1 repeatedly?" [examples] (\[x] -> Spec.Tests.reachesZero x)

    (_, nonlinfail) <- runner "Does a number reach zero, but not bigger than 4?" [examples] (\[x] -> Spec.Tests.reachesZeroFail x)

    hspec $ do
        -- biggerThan5spec -- 5 failures
        -- evenSpec        -- 5 failures
        dependentSpec      -- 0 failures, dependent on the first two
        doubleDependentSpec
        mv
        nonlin
        nonlinfail
