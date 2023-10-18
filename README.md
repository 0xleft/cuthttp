# cuthttp

A package to cut on the traffic needed. (Good for bruteforcing)

## Usage

```haskell
import Network.CutHTTP

main :: IO ()
main = do
    -- create server and request
    let server = Server "194.135.85.93" 80
    let request = Request GET "/" "HTTP/1.1" [("Header", "Header value")] "Hello"
    -- receive until word "Bad" or 2048 bytes
    let output = recvUntil request server "Bad" 2048

    print output
```