import Data.Set qualified as S
import SCGraph
import FlowAnalysis
import Lexer
import Parser
import SCT

import Test.Hspec

check :: String -> IO [String]
check file = do
    inp <- readFile file
    let decs = parse (alexScanTokens inp)
    scgset <- S.unions <$> mapM mkSCGraphs decs
    let fns = checkTermination scgset
    return $ map sigName fns

main :: IO ()
main = hspec $ do
    describe "Termination checking" $ do
        it "fibonacci" $ do
            let file = "test/01_fibonacci.txt"
            check file `shouldReturn` ["fib"]
        it "factorial" $ do
            let file = "test/02_factorize.txt"
            check file `shouldReturn` ["fact"]
        it "ackermann" $ do
            let file = "test/03_ackermann.txt"
            check file `shouldReturn` ["ack"]
        it "gcd" $ do
            let file = "test/04_gcd.txt"
            check file `shouldReturn` ["gcd"]
        it "countup" $ do
            let file = "test/05_countup.txt"
            check file `shouldReturn` []
        it "iseven and isodd" $ do
            let file = "test/06_iseven.txt"
            check file `shouldReturn` ["iseven", "isodd"]
        it "mutual" $ do
            let file = "test/07_mutual.txt"
            check file `shouldReturn` []
        it "sum_range" $ do
            let file = "test/08_sum_range.txt"
            check file `shouldReturn` []
        it "gcd_mod" $ do
            let file = "test/09_gcd_mod.txt"
            check file `shouldReturn` ["mod"]
        it "countdown" $ do
            let file = "test/10_countdown.txt"
            check file `shouldReturn` ["countdown"]
