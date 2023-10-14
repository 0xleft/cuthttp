import Test.Hspec
import CutHttp(makeRequest, recvUntil, Request(..), RequestType(..), Server(..))
import Network.Socket

main :: IO ()
main = hspec $ do
    describe "make server" $ do
        it "should return valid request" $ do
            let server = Server "localhost" 8080
            host server `shouldBe` "localhost"
            port server `shouldBe` 8080

    describe "make request string" $ do
        it "should parse request correctly" $ do
            let request = Request GET "/" "HTTP/1.1" [("Content-Type", "application/json")] "Hello" ""
            makeRequest request `shouldBe` "GET / HTTP/1.1\r\nContent-Type: application/json\r\n\r\nHello"

    describe "make socket" $ do
        it "should return valid socket" $ do
            let server = Server "194.135.85.93" 80
            let request = Request GET "/" "HTTP/1.1" [("Content-Type", "application/json")] "Hello" "Bad"
            recvUntil request server `shouldReturn` "HTTP/1.1 400 Bad"