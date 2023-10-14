module CutHttp(
    makeRequest
    , recvUntil
    , RequestType(..)
    , Request(..)
    , Server(..)
) where

import Network.Socket
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (unpack)
import Data.List (isInfixOf)

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
    host :: String,
    port :: Int
} deriving (Show, Eq)

makeHeaders :: [(String, String)] -> String
makeHeaders [] = ""
makeHeaders ((key, value):xs) = key ++ ": " ++ value ++ "\r\n" ++ makeHeaders xs

makeRequest :: Request -> String
makeRequest request = 
    show (requestType request) ++ " " ++ requestPath request ++ " " ++ requestVersion request ++ "\r\n" ++
    makeHeaders (requestHeaders request) ++
    "\r\n" ++
    requestBody request

recvUntil :: Request -> Server -> IO String
recvUntil request server = do
    addrInfo <- getAddrInfo Nothing (Just (host server)) (Just (show (port server)))
    let serverAddr = head addrInfo

    socket <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect socket (addrAddress serverAddr)

    let requestStr = makeRequest request
    send socket (BS.pack requestStr)

    response <- receiveInBatches socket (stopAt request)
    close socket
    return response

receiveInBatches :: Socket -> String -> IO String
receiveInBatches socket stopAt = receiveInBatches' socket stopAt ""

receiveInBatches' :: Socket -> String -> String -> IO String
receiveInBatches' socket stopAt accumulatedData = do
    response <- recv socket 1
    let responseStr = unpack response

    if BS.null response
        then return accumulatedData
        else if stopAt `isInfixOf` (accumulatedData ++ responseStr)
            then return (accumulatedData ++ responseStr)
            else receiveInBatches' socket stopAt (accumulatedData ++ responseStr)