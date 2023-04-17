{-# LANGUAGE GADTs #-}

module Spec.TestResult where

-- The test results are stored via type hiding, as not all testresults will have the same type
data TestResult = forall a. TestResult a