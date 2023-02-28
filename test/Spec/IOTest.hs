module Spec.IOTest where
    
import Numeric (readInt)
import Control.Monad.Except (MonadIO, liftIO)
import Test.Hspec
-- import System.Directory




getNumbers :: IO [Int]
getNumbers = do
    numbers <- readFile "./numbers.txt"
    return $ read numbers


spec :: (MonadIO m) => Int -> m (Maybe Int, Spec)
spec n = do
    inputNumbers <- liftIO getNumbers
    let test = all (>= n) inputNumbers 
    let returnspec = it (show n ++ " is not bigger than any element from IO input") $ test `shouldBe` True
    if test then 
        return (Just n, returnspec)
        else return (Nothing, returnspec)