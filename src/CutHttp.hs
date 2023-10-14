module CutHttp(
    RequestType(..)
    , Request(..)
    , Server(..)
) where

import Network.Socket

data RequestType = GET | POST | PUT | DELETE | HEAD | OPTIONS | CONNECT | TRACE | PATCH deriving (Show, Eq)

data Request = Request {
    requestType :: RequestType,
    requestPath :: String,
    requestVersion :: String,
    requestHeaders :: [(String, String)],
    requestBody :: String,
    stopAt :: String
} deriving (Show, Eq)

data Server = Server {
    serverPort :: Int,
    serverSocket :: Socket
} deriving (Show, Eq)

makeRequest :: Request -> String
makeRequest request = 
    let
        requestType = show $ CutHttp.requestType request
        requestPath = CutHttp.requestPath request
        requestVersion = CutHttp.requestVersion request
        requestHeaders = CutHttp.requestHeaders request
        requestBody = CutHttp.requestBody request
        stopAt = CutHttp.stopAt request
    in
        requestType ++ " " ++ requestPath ++ " " ++ requestVersion ++ "\r\n" ++ (makeHeaders requestHeaders) ++ "\r\n" ++ requestBody ++ stopAt