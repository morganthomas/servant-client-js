{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}


module Servant.Client.JS
  ( module Servant.Client.Core.Reexport
  ) where


import Control.Arrow
import Control.Concurrent
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.CaseInsensitive
import Data.Char
import Data.Foldable (toList)
import Data.Proxy
import Data.Functor.Alt
import qualified Data.Sequence as Seq
import Data.String.Conversions
import Foreign.StablePtr
import GHC.Generics
import qualified GHCJS.Buffer as Buffer
import GHCJS.Foreign.Callback
#ifdef ghcjs_HOST_OS
import GHCJS.Prim
import GHCJS.Types
#else
import "jsaddle" GHCJS.Prim
import "jsaddle" GHCJS.Types
#endif
import JavaScript.TypedArray.ArrayBuffer
import JavaScript.Web.Location
import Network.HTTP.Media (renderHeader)
import Network.HTTP.Types
import Servant.Client.Core
import Servant.Client.Core.Reexport
import qualified Servant.Types.SourceT as S


newtype JSXMLHttpRequest = JSXMLHttpRequest JSVal

newtype JSXMLHttpRequestClass = JSXMLHttpRequestClass JSVal

newtype ClientEnv = ClientEnv { baseUrl :: BaseUrl }
  deriving (Eq, Show)

newtype ClientM a = ClientM
  { runClientM' :: ReaderT ClientEnv (ExceptT ClientError IO) a }
  deriving ( Functor, Applicative, Monad, MonadIO, Generic
           , MonadReader ClientEnv, MonadError ClientError, MonadThrow
           , MonadCatch )

client :: HasClient ClientM api => Proxy api -> Client ClientM api
client api = api `clientIn` (Proxy :: Proxy ClientM)

instance MonadBase IO ClientM where
  liftBase = ClientM . liftBase

instance MonadBaseControl IO ClientM where
  type StM ClientM a = Either ClientError a

  liftBaseWith f = ClientM (liftBaseWith (\g -> f (g . runClientM')))

  restoreM st = ClientM (restoreM st)

instance Alt ClientM where
  a <!> b = a `catchError` const b

instance RunClient ClientM where
  runRequest = performRequest
  throwClientError = throwError

performRequest :: Request -> ClientM Response
performRequest req = do
  xhr <- liftIO initXhr
  burl <- asks baseUrl
  liftIO $ performXhr xhr burl req
  resp <- toResponse xhr

  let status = statusCode (responseStatusCode resp)
  unless (status >= 200 && status < 300) $ do
    let f b = (burl, BL.toStrict $ toLazyByteString b)
    throwError $ FailureResponse (bimap (const ()) f req) resp

  pure resp

initXhr :: IO JSXMLHttpRequest
initXhr = do
  lib <- requireXMLHttpRequestClass
  newXMLHttpRequest lib

foreign import javascript unsafe "XMLHttpRequest"
  requireXMLHttpRequestClass :: IO JSXMLHttpRequestClass

foreign import javascript unsafe "new $1()"
  newXMLHttpRequest :: JSXMLHttpRequestClass -> IO JSXMLHttpRequest

performXhr :: JSXMLHttpRequest -> BaseUrl -> Request -> IO ()
performXhr xhr burl request = do

  waiter <- newEmptyMVar

  bracket (acquire waiter) releaseCallback $ \_callback -> do
        t <- myThreadId
        s <- newStablePtr t

        openXhr xhr (cs $ requestMethod request) (toUrl burl request) True
        setHeaders xhr request
        js_setResponseType xhr "arraybuffer"
        body <- toBody request
        sendXhr xhr body
        takeMVar waiter

        freeStablePtr s
  where
    acquire waiter = onReadyStateChange xhr $ do
      state <- readyState xhr
      case state of
        -- onReadyStateChange's callback can fire state 4
        -- (which means "request finished and response is ready")
        -- multiple times. By using tryPutMVar, only the first time
        -- state 4 is fired will cause an MVar to be put. Subsequent
        -- fires are ignored.
        4 -> void $ tryPutMVar waiter ()
        _ -> return ()

onReadyStateChange :: JSXMLHttpRequest -> IO () -> IO (Callback (IO ()))
onReadyStateChange xhr action = do
  callback <- asyncCallback action
  js_onReadyStateChange xhr callback
  return callback
foreign import javascript safe "$1.onreadystatechange = $2;"
  js_onReadyStateChange :: JSXMLHttpRequest -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$1.readyState"
  readyState :: JSXMLHttpRequest -> IO Int

openXhr :: JSXMLHttpRequest -> String -> String -> Bool -> IO ()
openXhr xhr method url =
  js_openXhr xhr (toJSString method) (toJSString url)
foreign import javascript unsafe "$1.open($2, $3, $4)"
  js_openXhr :: JSXMLHttpRequest -> JSVal -> JSVal -> Bool -> IO ()

foreign import javascript unsafe "$1.responseType = $2;"
  js_setResponseType :: JSXMLHttpRequest -> JSString -> IO ()

toUrl :: BaseUrl -> Request -> String
toUrl burl request =
  let pathS = cs $ toLazyByteString $ requestPath request
      queryS =
          cs $
          renderQuery True $
          toList $
          requestQueryString request
  in showBaseUrl burl ++ pathS ++ queryS

setHeaders :: JSXMLHttpRequest -> Request -> IO ()
setHeaders xhr request = do
  forM_ (toList $ requestAccept request) $ \mediaType ->
    js_setRequestHeader
      xhr
      (toJSString "Accept")
      (toJSString $ cs $ renderHeader mediaType)

  forM_ (requestBody request) $ \(_, mediaType) ->
    js_setRequestHeader
      xhr
      (toJSString "Content-Type")
      (toJSString $ cs $ renderHeader mediaType)

  forM_ (toList $ requestHeaders request) $ \(key, value) ->
    js_setRequestHeader xhr (toJSString $ cs $ original key) (toJSString $ cs value)

foreign import javascript unsafe "$1.setRequestHeader($2, $3)"
  js_setRequestHeader :: JSXMLHttpRequest -> JSVal -> JSVal -> IO ()

sendXhr :: JSXMLHttpRequest -> Maybe ArrayBuffer -> IO ()
sendXhr xhr Nothing = js_sendXhr xhr
sendXhr xhr (Just body) =
  js_sendXhrWithBody xhr body

foreign import javascript unsafe "$1.send()"
  js_sendXhr :: JSXMLHttpRequest -> IO ()

foreign import javascript unsafe "$1.send($2)"
  js_sendXhrWithBody :: JSXMLHttpRequest -> ArrayBuffer -> IO ()

toBody :: Request -> IO (Maybe ArrayBuffer)
toBody request = case requestBody request of
  Nothing -> return Nothing
  Just (a, _) -> Just <$> go a

  where
    go :: RequestBody -> IO ArrayBuffer
    go x = case x of
      RequestBodyLBS x     -> return $ mBody $ BL.toStrict x
      RequestBodyBS x      -> return $ mBody x
      RequestBodySource xs -> runExceptT (S.runSourceT xs) >>= \e -> case e of
        Left err  -> fail err
        Right bss -> return $ mBody $ BL.toStrict $ mconcat bss

    mBody :: BS.ByteString -> ArrayBuffer
    mBody bs = js_bufferSlice offset len $ Buffer.getArrayBuffer buffer
      where
        (buffer, offset, len) = Buffer.fromByteString bs

foreign import javascript unsafe "$3.slice($1, $1 + $2)"
  js_bufferSlice :: Int -> Int -> ArrayBuffer -> ArrayBuffer

-- * inspecting the xhr response

-- This function is only supposed to handle 'ConnectionError's. Other
-- 'ClientError's are created in Servant.Client.Req.
toResponse :: JSXMLHttpRequest -> ClientM Response
toResponse xhr = do
  status <- liftIO $ getStatus xhr
  case status of
    0 -> throwError $ ConnectionError (SomeException (userError "connection error"))
    _ -> liftIO $ do
      statusText <- cs <$> getStatusText xhr
      headers <- parseHeaders <$> getAllResponseHeaders xhr
      response <- getResponse xhr
      pure Response
        { responseStatusCode = mkStatus status statusText
        , responseBody = response
        , responseHeaders = Seq.fromList headers
        , responseHttpVersion = http11 -- this is made up
        }

foreign import javascript unsafe "$1.status"
  getStatus :: JSXMLHttpRequest -> IO Int

getStatusText :: JSXMLHttpRequest -> IO String
getStatusText = fmap fromJSString . js_statusText
foreign import javascript unsafe "$1.statusText"
  js_statusText :: JSXMLHttpRequest -> IO JSVal

getAllResponseHeaders :: JSXMLHttpRequest -> IO String
getAllResponseHeaders xhr =
  fromJSString <$> js_getAllResponseHeaders xhr
foreign import javascript unsafe "$1.getAllResponseHeaders()"
  js_getAllResponseHeaders :: JSXMLHttpRequest -> IO JSVal

getResponse :: JSXMLHttpRequest -> IO BL.ByteString
getResponse xhr =
    BL.fromStrict
  . Buffer.toByteString 0 Nothing
  . Buffer.createFromArrayBuffer
  <$> js_response xhr

foreign import javascript unsafe "$1.response"
  js_response :: JSXMLHttpRequest -> IO ArrayBuffer

parseHeaders :: String -> ResponseHeaders
parseHeaders s =
  first mk . first strip . second strip . parseHeader <$>
  splitOn "\r\n" (cs s)
  where
    parseHeader :: BS.ByteString -> (BS.ByteString, BS.ByteString)
    parseHeader h = case BS.breakSubstring ":" (cs h) of
      (key, BS.drop 1 -> value) -> (key, value)

    splitOn :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
    splitOn separator input = case BS.breakSubstring separator input of
      (prefix, "") -> [prefix]
      (prefix, rest) -> prefix : splitOn separator (BS.drop (BS.length separator) rest)

    strip :: BS.ByteString -> BS.ByteString
    strip = BS.dropWhile isSpace . BS.reverse . BS.dropWhile isSpace . BS.reverse
