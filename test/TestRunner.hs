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

  -}

dependencyTesting :: Eq a => Monad m => ([Maybe b], Spec, [(a, Maybe b)])  -- Collection of result list
                             -> [Maybe a]                          -- Values to be tested
                             -> (a -> Maybe [a])                     -- DependencyFunction  
                             -> (a -> m (Maybe b, Spec))           -- spectest
                             -> m ([Maybe b], Spec, [(a, Maybe b)]) 
dependencyTesting steps [] _ _ = return steps                        -- end of recursion
dependencyTesting (bs, specsequence, resMap) (Nothing:as) depFunc spectest = dependencyTesting (Nothing:bs, specsequence, resMap) as depFunc spectest -- skip testing values that are `Nothing` 
dependencyTesting (bs, specsequence, resMap) (Just x : as) depFunc spectest = 
  case lookup x resMap of
    --either the test already ran and was good, then we can add the result to the list of bs: 
    Just res  -> dependencyTesting (res:bs, specsequence, resMap) as depFunc spectest
    --or not, now we need to test all its dependencies before x:
    Nothing ->
      let dependencies = depFunc x 
      in case dependencies of
        Nothing -> do                                  -- if no deps, simply test and add result to bs, add spec to specsequence and add the mapping x->b to the resultmap
           (b, spec) <- spectest x 
           dependencyTesting (b:bs, spec >> specsequence, (x, b):resMap) as depFunc spectest
        Just deps -> do
          (bs', specsequence', resMap') <- dependencyTesting ([], specsequence, resMap) (map Just deps) depFunc spectest
          if all isJust bs' 
            then do 
              (b, spec) <- spectest x 
              dependencyTesting (b:bs, spec >> specsequence, (x, b): resMap ++ resMap') as depFunc spectest
            else dependencyTesting (Nothing:bs, specsequence, resMap) as depFunc spectest




runnerNonLinear :: Eq a => Monad m 
                  => Description
                  -> [Maybe a]                     -- Values to be tested
                  -> (a -> Maybe [a])                -- Function for dependencies
                  -> (a -> m (Maybe b, Spec))
                  -> m ([Maybe b], Spec)
runnerNonLinear descr exs depFunc spectest = do                      
  tested <- dependencyTesting ([], return (), []) exs depFunc spectest
  case tested of
    (bs, specs, _) -> return (bs, describe descr specs)
                    



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

    --(_, nonlin) <- runner "Does a number reach zero if you substract 1 repeatedly?" [examples] (\[x] -> Spec.Tests.reachesZero x)

    --(_, nonlinfail) <- runner "Does a number reach zero, but not bigger than 4?" [examples] (\[x] -> Spec.Tests.reachesZeroFail x)

    (_, runNonLin) <- runnerNonLinear "Testing nonlinear runner" examples (\x -> if x == 0 then Nothing else Just [x-1]) Spec.Tests.reachesZero

    (_, runNonLinFail) <- runnerNonLinear "Testing nonlinear runner failing a test" examples (\x -> if x == 0 then Nothing else Just [x-1]) Spec.Tests.reachesZeroFail
    hspec $ do
        -- biggerThan5spec -- 5 failures
        -- evenSpec        -- 5 failures
        dependentSpec      -- 0 failures, dependent on the first two
        doubleDependentSpec
        mv
        runNonLin
        runNonLinFail
