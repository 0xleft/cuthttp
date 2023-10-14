import Test.Hspec
import CutHttp(makeRequest, Request(..), RequestType(..), Server(..))

main :: IO ()
main = hspec $ do
  describe "make request" $ do
    it "should return valid request" $ do
        let server = Server 8080 undefined
        server `shouldBe` Server 8080 undefined        