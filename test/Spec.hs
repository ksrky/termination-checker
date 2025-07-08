import Data.Set     qualified as S
import FlowAnalysis
import Lexer
import Parser
import SCGraph
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
            check file `shouldReturn` []
        it "factorial" $ do
            let file = "test/02_factorize.txt"
            check file `shouldReturn` []
        it "ackermann" $ do
            let file = "test/03_ackermann.txt"
            check file `shouldReturn` []
        it "gcd" $ do
            let file = "test/04_gcd.txt"
            check file `shouldReturn` []
        it "countup" $ do
            let file = "test/05_countup.txt"
            check file `shouldReturn` ["count"]
        it "iseven and isodd" $ do
            let file = "test/06_iseven.txt"
            check file `shouldReturn` []
        it "mutual" $ do
            let file = "test/07_mutual.txt"
            check file `shouldReturn` ["f", "g"]
        it "sum_range" $ do
            let file = "test/08_sum_range.txt"
            check file `shouldReturn` ["sum_range"]
        it "gcd_mod" $ do
            let file = "test/09_gcd_mod.txt"
            check file `shouldReturn` ["gcd_mod"]
        it "countdown" $ do
            let file = "test/10_countdown.txt"
            check file `shouldReturn` []
        it "tarai" $ do
            let file = "test/11_tarai.txt"
            check file `shouldReturn` ["tarai"]
        it "looppend" $ do
            let file = "test/12_looppend.txt"
            check file `shouldReturn` []
