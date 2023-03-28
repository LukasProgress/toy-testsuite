{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Except (forM)
import Control.Monad ( foldM, join )

import Test.Hspec ( hspec, describe, Spec, pending, it )

import Spec.Tests
import Spec.IOTest ( spec )
import Spec.HorizontalDependency
import Data.Maybe (fromJust, isJust, catMaybes, isNothing)
import Data.List (transpose, delete)
import Control.Monad.Reader
import Control.Monad.State
import Unsafe.Coerce ( unsafeCoerce )
import Control.Monad.RWS (Any(Any))

type Description = String

-----------------------------------------------------------------------------------
---------Building it as a Monad-----------

-- The config can be set when building a new TestM, 

-- DefConf      skips all tests, for which vertical dependencies are not fullfilled
-- PendingConf  shows those tests as pending 
data Config = DefConf | PendingConf

-- The test results are stored via type hiding, as not all testresults will have the same type
data TestResult = forall b. TestResult [Maybe b]
-- All results will also be stored with an index, which can be used to define horizontal dependencies
type TestResults = [(Int, TestResult)]

-- The Teststate:
data TestState = TestState {count :: Int                  -- current highest index of testresults
                         , tests :: TestResults           -- testresults of previous tests
                         , testSpecs :: Spec              -- the spectests, ready to be run
                         }

initialTestState :: TestState
initialTestState = TestState {count = 0, tests = [], testSpecs = return ()}

-- TestState Monad: 
-- A Monadstack now bringing the TestState and Config together
newtype TestM m a = MkTestM {runTestM :: ReaderT Config (StateT TestState m) a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadState TestState, MonadReader Config)


liftTestM :: (Monad m) => m a -> TestM m a
liftTestM = MkTestM . lift . lift

------------------------------------------
-- Helper functions for testing: 

-- Add a testresult (tuple of results and spec) to the state
addTestResult :: MonadState TestState m => ([Maybe b], Spec) -> m ()
addTestResult (result, spec) = modify (\s -> s {count = count s + 1, 
                                                tests = (count s, TestResult result) : tests s, 
                                                testSpecs = testSpecs s >> spec})

-- get the current number in count
getTestId :: MonadState TestState m => m Int
getTestId = gets count


-- For horizontal dependencies: bringing together the current examples to be tested (exs)
-- and filter them, so that only those for which the deps are not Nothing are kept as `Just` values
zipExamplesWithDeps :: [Maybe a] -> [[Maybe Any]] -> [Maybe a]
zipExamplesWithDeps exs deps = case exs of 
  [] -> []
  (Nothing : xs) -> Nothing : zipExamplesWithDeps xs (map tail deps)
  (Just x : xs) -> let depsAtIndex = map head deps 
                   in (if all isJust depsAtIndex then Just x else Nothing)
                      : zipExamplesWithDeps xs (map tail deps)

-- filter examples with a list of ids, which are the ids for horizontal dependencies
extractDeps :: MonadState TestState m => [Int] -> [Maybe a] -> m [Maybe a]
extractDeps ids exs = do 
  stateTests <- gets tests
  let deps = map (\id -> case lookup id stateTests of
                          Just (TestResult res) -> unsafeCoerce res :: [Maybe Any])
                 ids 
      result = zipExamplesWithDeps exs deps
  return result


------------------------------------------
-- The core of this library - runTest

-- Arguments: 
-- descr          A description for the test
-- exs            A list of examples to be tested. Nothing values will be skipped
-- dependencies   A tuple with a dependency function (calculating a list of vertical dependencies)
--                       , and a List of indices, serving as horizontal dependencies
-- spectest       The spectest that is to be run over every example, returning a result and a spec

-- Returns: An tuple (Int, [Maybe b]), which depict an index for these testresults (to give to other tests as horizontal dep.) and
--                                                  a List of the results (if a test was not successfull, its index will be filled 
--                                                                         with a `Nothing`)
runTest :: (Eq a) => Monad m =>
                    Description
                  -> [Maybe a]                     -- Values to be tested
                  -> (a -> Maybe [a], [Int])                -- Function for dependencies
                  -> (a -> m (Maybe b, Spec))
                  -> TestM m (Int, [Maybe b])                   -- returns just the id of the test
runTest descr exs (depFunc, depIds) spectest = do
  conf <- ask
  horizontalDepTests <- extractDeps depIds exs

  tested <- dependencyTestingM [] horizontalDepTests depFunc spectest

  let results = case conf of
                  DefConf -> map (join . fmap (`lookup` tested)) exs
                  PendingConf -> map (\ex -> case ex of 
                                          Nothing -> Nothing
                                          Just val -> case lookup val tested of
                                              Nothing -> Just (Nothing, it "The dependencies were not fullfilled" $ do pending)
                                              Just x -> Just x) 
                                 exs
  -- extract results of test runs
  let bs = map (join . fmap fst) results
  -- extract and combine test display output
  let specs = mapM_ snd (catMaybes results)
  testId <- getTestId
  addTestResult (bs, describe descr specs)
  return (testId, bs)


-- The algorithm for vertical dependencies. Vertical dependencies are recursively calculated
-- TODO: MAYBE it is possible to remember dependencies across different tests? But I am not sure, 
-- As we cant reconstruct the types
dependencyTestingM :: Eq a => Monad m =>
                             [(a, (Maybe b, Spec))]  -- Collection of result list
                             -> [Maybe a]                          -- Values to be tested
                             -> (a -> Maybe [a])                     -- DependencyFunction  
                             -> (a -> m (Maybe b, Spec))           -- spectest
                             -> TestM m [(a, (Maybe b, Spec))]
dependencyTestingM steps [] _ _ = return steps
dependencyTestingM steps (Nothing: as) depFunc spectest = dependencyTestingM steps as depFunc spectest                  
dependencyTestingM resMap (Just x : as) depFunc spectest =
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
          resMap' <- dependencyTestingM resMap (map Just deps) depFunc spectest
          let dependenciesFullfilled = all (\dep -> case lookup dep resMap' of
                                                     Just (Just _, _) -> True
                                                     _                -> False)
                                            deps
          if dependenciesFullfilled then do
                                      testResult <- liftTestM $ spectest x
                                      dependencyTestingM ((x, testResult):resMap) as depFunc spectest
                                    else dependencyTestingM resMap' as depFunc spectest


-- An example for the usage of a TestM Monad:
-----------------Put all tests to be run here: ---------------------
testM :: TestM IO TestState
testM = do
  let examples = map Just [1..20]
      minusOneDepFunc x = if x == 0 then Nothing else Just [x-1]
      timesTwoDepFunc x = if x < 30 then Just[x * 2] else Nothing
  -- testing vertical dependencies: 
  (test1, res) <- runTest "Testing Pendingconf (bigger than 5 should be pending)" examples (minusOneDepFunc, []) Spec.Tests.reachesZeroFail 
  --- testing horizontal dependencies: 
  (vert1, res1) <- runTest "`parsing` a file -> n < 3 is supposed to fail" examples (const Nothing, []) Spec.HorizontalDependency.parseTest
  (vertTest, res2) <- runTest "Testing out direct dependency, working with results" res1 (const Nothing, []) Spec.HorizontalDependency.typechecktest
  (vertTest2, _) <- runTest "Testing out vertical dependencies " res2 (const Nothing, [vertTest, test1]) Spec.HorizontalDependency.someOthertest
  depFuncTest <- runTest "Testing out dependency function x * 2" examples (timesTwoDepFunc, []) Spec.HorizontalDependency.parseTest  
  test3 <- runTest "Testing out everything together" examples (timesTwoDepFunc, [vertTest2]) Spec.HorizontalDependency.parseTest
  get

---------------------------------------------------------------------

main :: IO ()
main = do
    --------- Run tests with the TestM Monad --------------

    let config = DefConf
        testState = runReaderT (runTestM testM) config

    finalState <- evalStateT testState initialTestState

    hspec $ do
      -- We could theoretically create multiple TestM and run their final testStates here:
      testSpecs finalState
