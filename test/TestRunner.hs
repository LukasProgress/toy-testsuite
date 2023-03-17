{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Except (forM)
import Control.Monad ( foldM, join )

import Test.Hspec ( hspec, describe, Spec )

import Spec.Tests
import Spec.IOTest ( spec )
import Data.Maybe (fromJust, isJust, catMaybes)
import Data.List (transpose, delete)
import Control.Monad.Reader
import Control.Monad.State

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


---------------------------- Non linear runner ---------------------

dependencyTesting :: Eq a => Monad m => [(a, (Maybe b, Spec))]  -- Collection of result list
                             -> [a]                          -- Values to be tested
                             -> (a -> Maybe [a])                     -- DependencyFunction  
                             -> (a -> m (Maybe b, Spec))           -- spectest
                             -> m [(a, (Maybe b, Spec))]
dependencyTesting steps [] _ _ = return steps                        -- end of recursion
dependencyTesting resMap (x : as) depFunc spectest =
  case lookup x resMap of
    --either the test already ran, then we can add the result to the list of bs: 
    Just _  -> dependencyTesting resMap as depFunc spectest
    --or not, now we need to test all its dependencies before x:
    Nothing ->
      let dependencies = depFunc x
      in case dependencies of
        Nothing -> do                                  -- if no deps, simply test and add result to bs, add spec to specsequence and add the mapping x->b to the resultmap
           testResult <- spectest x
           dependencyTesting  ((x, testResult):resMap) as depFunc spectest
        Just deps -> do
          resMap' <- dependencyTesting resMap deps depFunc spectest
          let dependenciesFullfilled = all (\dep -> case lookup dep resMap' of
                                                     Just (Just _, _) -> True
                                                     _                -> False)
                                            deps
          if dependenciesFullfilled then do
                                      testResult <- spectest x
                                      dependencyTesting  ((x, testResult):resMap) as depFunc spectest
                                    else dependencyTesting resMap' as depFunc spectest





runnerNonLinear :: Eq a => Monad m
                  => Description
                  -> [Maybe a]                     -- Values to be tested
                  -> (a -> Maybe [a])                -- Function for dependencies
                  -> (a -> m (Maybe b, Spec))
                  -> m ([Maybe b], Spec)
runnerNonLinear descr exs depFunc spectest = do
  -- get map of all test runs, including all dependencies
  tested <- dependencyTesting [] (catMaybes exs) depFunc spectest
  -- lookup results that belong to inputs
  let results = map (join . fmap (`lookup` tested)) exs
  -- extract results of test runs
  let bs = map (join . fmap fst) results
  -- extract and combine test display output
  let specs = mapM_ snd (catMaybes results)
  return (bs, describe descr specs)


-----------------------------------------------------------------------------------
---------Building it as a Monad-----------
data Config = DefConf | PendingConf

data TestResult = forall a. TestResult (Int, [Maybe a])

type TestResults = [TestResult]

data TestState = TestState {count :: Int
                         , tests :: TestResults
                         , testSpecs :: Spec
                         }

initialTestState :: TestState
initialTestState = TestState {count = 0, tests = [], testSpecs = return ()}

-- TestState Monad: 
newtype TestM m a = MkTestM {runTestM :: ReaderT Config (StateT TestState m) a}
  deriving (Applicative, Functor, Monad, MonadIO)


liftTestM :: (Monad m) => m a -> TestM m a
liftTestM = MkTestM . lift . lift

------------------------------------------
addTestResult :: MonadState TestState m => ([Maybe b], Spec) -> m ()
addTestResult (result, spec) = modify (\s -> s {count = count s, 
                                                tests = TestResult (count s, result) : tests s, 
                                                testSpecs = spec >> testSpecs s})
------------------------------------------

runTest :: (Eq a, MonadState TestState (TestM m), MonadReader a0 (TestM m)) => Monad m =>
                    Description
                  -> [Maybe a]                     -- Values to be tested
                  -> (a -> Maybe [a])                -- Function for dependencies
                  -> (a -> m (Maybe b, Spec))
                  -> TestM m ([Maybe b], Spec)
runTest descr exs depFunc spectest = do
  -- get map of all test runs, including all dependencies
  tested <- dependencyTestingM [] (catMaybes exs) depFunc spectest
  -- lookup results that belong to inputs
  conf <- ask
  -- TODO: Somehow get the config to build in pending tests
  let results = map (join . fmap (`lookup` tested)) exs
  -- extract results of test runs
  let bs = map (join . fmap fst) results
  -- extract and combine test display output
  let specs = mapM_ snd (catMaybes results)
  addTestResult (bs, describe descr specs)
  return (bs, describe descr specs)

dependencyTestingM :: Eq a => Monad m =>
                             [(a, (Maybe b, Spec))]  -- Collection of result list
                             -> [a]                          -- Values to be tested
                             -> (a -> Maybe [a])                     -- DependencyFunction  
                             -> (a -> m (Maybe b, Spec))           -- spectest
                             -> TestM m [(a, (Maybe b, Spec))]
dependencyTestingM steps [] _ _ = return steps                        -- end of recursion
dependencyTestingM resMap (x : as) depFunc spectest =
  case lookup x resMap of
    --either the test already ran, then we can add the result to the list of bs: 
    Just _  -> dependencyTestingM resMap as depFunc spectest
    --or not, now we need to test all its dependencies before x:
    Nothing ->
      let dependencies = depFunc x
      in case dependencies of
        Nothing -> do                                  -- if no deps, simply test and add result to bs, add spec to specsequence and add the mapping x->b to the resultmap
           testResult <- liftTestM $ spectest x
           dependencyTestingM ((x, testResult):resMap) as depFunc spectest
        Just deps -> do
          resMap' <- dependencyTestingM resMap deps depFunc spectest
          let dependenciesFullfilled = all (\dep -> case lookup dep resMap' of
                                                     Just (Just _, _) -> True
                                                     _                -> False)
                                            deps
          if dependenciesFullfilled then do
                                      testResult <- liftTestM $ spectest x
                                      dependencyTestingM ((x, testResult):resMap) as depFunc spectest
                                    else dependencyTestingM resMap' as depFunc spectest


main :: IO ()
main = do
    let examples = map Just [1..10]
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

    let minusOneDepFunc x = if x == 0 then Nothing else Just [x-1]

    (_, runNonLin) <- runnerNonLinear "Testing nonlinear runner" examples minusOneDepFunc Spec.Tests.reachesZero

    (_, runNonLinFail) <- runnerNonLinear "Testing nonlinear runner failing a test" examples minusOneDepFunc Spec.Tests.reachesZeroFail

    (_, runNonLinFail2) <- runnerNonLinear "Second example with different example list" (map Just [1, 3]) minusOneDepFunc Spec.Tests.reachesZeroFail
    
    --------- Run tests with the TestM Monad --------------
    
    let config = DefConf
    let testM = runTest "Testing nonlinear runner failing a test" examples minusOneDepFunc Spec.Tests.reachesZeroFail

    hspec $ do
        -- biggerThan5spec -- 5 failures
        -- evenSpec        -- 5 failures
        dependentSpec      -- 0 failures, dependent on the first two
        doubleDependentSpec
        mv
        runNonLin
        runNonLinFail
        runNonLinFail2
