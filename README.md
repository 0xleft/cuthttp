# cuthttp

A package to cut on the traffic needed.

## Usage

```haskell
import Network.CutHTTP

main :: IO ()
main = do
    let server = Server "194.135.85.93" 80
    let request = Request GET "/" "HTTP/1.1" [("Header", "Header value")] "Hello"
    let output = recvUntil request server "Bad" 2048

    print output
```