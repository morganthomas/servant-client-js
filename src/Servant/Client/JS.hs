{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wunused-imports        #-}
{-# OPTIONS_GHC -Wincomplete-patterns   #-}


-- TODO: make this safe for binary / non-UTF8


module Servant.Client.JS
  ( module Servant.Client.Core.Reexport
  , ClientEnv (..)
  , ClientM (..)
  , runClientM
  , client
  , withStreamingRequestJSM
  ) where


import Control.Concurrent
import Control.Monad (forM_)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except
import Data.Binary.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.CaseInsensitive
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Functor.Alt
import qualified Data.Sequence as Seq
import Data.Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Conc
import GHC.Generics
#ifdef ghcjs_HOST_OS
import GHCJS.Prim hiding (getProp, fromJSString)
import Language.Javascript.JSaddle (fromJSString)
#else
import "jsaddle" GHCJS.Prim hiding (fromJSString)
#endif 
import Language.Javascript.JSaddle (
#ifndef ghcjs_HOST_OS
  MonadJSM,
#endif
  JSM, liftJSM, jsg, toJSVal, obj, (#), (<#), fun, fromJSVal, (!), listProps, getProp, JSString (..), makeObject, isTruthy, ghcjsPure )
import Network.HTTP.Media (renderHeader)
import Network.HTTP.Types
import Servant.Client.Core
import Servant.Client.Core.Reexport
import qualified Servant.Types.SourceT as S

default (Text)


newtype JSXMLHttpRequest = JSXMLHttpRequest JSVal

newtype JSXMLHttpRequestClass = JSXMLHttpRequestClass JSVal

newtype ClientEnv = ClientEnv { baseUrl :: BaseUrl }
  deriving (Eq, Show)

newtype ClientM a = ClientM
  { runClientM' :: ReaderT ClientEnv (ExceptT ClientError JSM) a }
  deriving ( Functor, Applicative, Monad, MonadIO
#ifndef ghcjs_HOST_OS
           , MonadJSM
#endif
           , Generic, MonadReader ClientEnv, MonadError ClientError
           , MonadThrow, MonadCatch )

client :: HasClient ClientM api => Proxy api -> Client ClientM api
client api = api `clientIn` (Proxy :: Proxy ClientM)

runClientM :: ClientM a -> ClientEnv -> JSM (Either ClientError a)
runClientM m env = runExceptT $ runReaderT (runClientM' m) env

#ifndef ghcjs_HOST_OS
instance MonadBase JSM JSM where
  liftBase = id
#endif

instance MonadBase JSM ClientM where
  liftBase = ClientM . liftBase

#ifndef ghcjs_HOST_OS
instance MonadBaseControl JSM JSM where
  type StM JSM a = a
  liftBaseWith f = f id
  restoreM = return
#endif

instance MonadBaseControl JSM ClientM where
  type StM ClientM a = Either ClientError a

  liftBaseWith f = ClientM (liftBaseWith (\g -> f (g . runClientM')))

  restoreM st = ClientM (restoreM st)

instance Alt ClientM where
  a <!> b = a `catchError` const b

instance RunClient ClientM where
  runRequest = fetch
  throwClientError = throwError

instance RunStreamingClient ClientM where
  withStreamingRequest req handler = withStreamingRequestJSM req (liftIO . handler)


#ifdef ghcjs_HOST_OS
unJSString :: JSString -> Text
unJSString = fromJSString
#else
unJSString :: JSString -> Text
unJSString (JSString s) = s
#endif


getFetchArgs :: ClientEnv -> Request -> JSM [JSVal]
getFetchArgs (ClientEnv (BaseUrl urlScheme host port basePath))
          (Request reqPath reqQs reqBody reqAccept reqHdrs _reqVer reqMethod) = do
  window <- liftJSM $ jsg "window"
  let schemeStr :: Text
      schemeStr = case urlScheme of
                    Http -> "http://"
                    Https -> "https://"
  url <- toJSVal $ schemeStr <> pack host <> ":" <> pack (show port) <> pack basePath
                             <> decodeUtf8 (BL.toStrict (toLazyByteString reqPath))
                             <> (if Prelude.null reqQs then "" else "?" ) <> (intercalate "&" 
                                        $ (\(k,v) -> decodeUtf8 k <> "="
                                                           <> maybe "" decodeUtf8 v)
                                         <$> Prelude.foldr (:) [] reqQs)
  init <- liftJSM obj
  methodStr <- liftJSM . toJSVal $ decodeUtf8 reqMethod
  liftJSM $ init <# "method" $ methodStr
  headers <- liftJSM obj
  forM_  reqHdrs $ \(original -> k, v) -> do
    v' <- liftJSM $ toJSVal (decodeUtf8 v)
    liftJSM $ headers <# decodeUtf8 k $ v'
  forM_ reqAccept $ \mt -> do
    mt' <- liftJSM $ toJSVal (decodeUtf8 (renderHeader mt))
    liftJSM $ headers <# "Accept" $ mt'
  liftJSM $ init <# "headers" $ headers
  case reqBody of
    Just (RequestBodyLBS x, mt) -> do
      v <- liftJSM $ toJSVal (decodeUtf8 (BL.toStrict x))
      liftJSM $ init <# "body" $ v
      mt' <- liftJSM $ toJSVal (decodeUtf8 (renderHeader mt))
      liftJSM $ headers <# "Content-Type" $ mt'
    Just (RequestBodyBS x, mt) -> do
      v <- liftJSM $ toJSVal (decodeUtf8 x)
      liftJSM $ init <# "body" $ v
      mt' <- liftJSM $ toJSVal (decodeUtf8 (renderHeader mt))
      liftJSM $ headers <# "Content-Type" $ mt'
    Just (RequestBodySource _, _) -> error "Servant.Client.JS.withStreamingRequest(JSM) does not (yet) support RequestBodySource"
    Nothing -> return ()
  init' <- liftJSM $ toJSVal init
  return [url, init']


getResponseMeta :: JSVal -> JSM (Status, Seq.Seq Header, HttpVersion)
getResponseMeta res = do
  status <- toEnum . fromMaybe 200
            <$> (liftJSM $ (fromJSVal =<< res ! ("status" :: Text)))
  resHeadersObj <- makeObject =<< res ! ("headers" :: Text)
  resHeaderNames <- liftJSM $ listProps resHeadersObj
  resHeaders <- fmap (Prelude.foldr (Seq.:<|) Seq.Empty)
             .  forM resHeaderNames $ \headerName -> do
    headerValue <- fmap (fromMaybe "") . liftJSM . fromJSVal 
                   =<< liftJSM (getProp headerName resHeadersObj)
    return (mk (encodeUtf8 (unJSString headerName)), encodeUtf8 headerValue)
  return (status, resHeaders, http11) -- http11 is made up


parseChunk :: JSVal -> JSM (Maybe BS.ByteString)
parseChunk chunk = do
  isDone <- ghcjsPure =<< isTruthy
              <$> (chunk ! ("isDone" :: Text))
  case isDone of
    True -> return Nothing
    False -> fmap encodeUtf8 <$> (liftJSM $ fromJSVal =<< chunk ! ("value" :: Text))



fetch :: Request -> ClientM Response
fetch req = ClientM . ReaderT $ \env -> do
  window <- liftJSM $ jsg ("window" :: Text)
  args <- liftJSM $ getFetchArgs env req
  promise <- liftJSM $ window # ("fetch" :: Text) $ args
  contents <- liftIO $ newTVarIO (mempty :: BS.ByteString)
  result <- liftIO newEmptyMVar
  promiseHandler <- liftJSM . toJSVal . fun $ \_ _ args ->
    case args of
      [res] -> do
        meta <- getResponseMeta res
        stream <- res ! ("body" :: Text)
        rdr <- stream # ("getReader" :: Text) $ ([] :: [JSVal])
        _ <- fix $ \go -> do
          rdrPromise <- liftJSM $ res # ("getReader" :: Text) $ ([] :: [JSVal])
          rdrHandler <- toJSVal . fun $ \_ _ args ->
            case args of
              [chunk] -> do
                next <- liftJSM $ parseChunk chunk
                case next of
                  Nothing -> liftIO $ putMVar result . (meta,) =<< readTVarIO contents
                  Just x -> do
                    liftIO . atomically $ writeTVar contents . (<> x) =<< readTVar contents
                    go
              _ -> error "fetch read promise handler received wrong number of arguments"
          _ <- rdrPromise # ("then" :: Text) $ [rdrHandler]
          return ()
        return ()
      _ -> error "fetch promise handler received wrong number of arguments"
  ((status, hdrs, ver), body) <- liftIO $ takeMVar result
  return $ Response status hdrs ver (BL.fromStrict body)


withStreamingRequestJSM :: Request -> (StreamingResponse -> JSM a) -> ClientM a
withStreamingRequestJSM req handler =
  ClientM . ReaderT $ \env -> do
    window <- liftJSM $ jsg "window"
    fetchArgs <- liftJSM $ getFetchArgs env req
    fetchPromise <- liftJSM $ window # "fetch" $ fetchArgs
    push <- liftIO newEmptyMVar
    result <- liftIO newEmptyMVar
    fetchPromiseHandler <- liftJSM . toJSVal . fun $ \_ _ args ->
      case args of
        [res] -> do
          (status, hdrs, ver) <- getResponseMeta res
          rdr <- liftJSM $ res # ("getReader" :: Text) $ ([] :: [JSVal])
          _ <- fix $ \go -> do
            rdrPromise <- liftJSM $ rdr # ("read" :: Text) $ ([] :: [JSVal])
            rdrHandler <- toJSVal . fun $ \_ _ args ->
              case args of
                [chunk] -> do
                  isDone <- liftJSM $ ghcjsPure =<< isTruthy
                              <$> (chunk ! ("isDone" :: Text))
                  case isDone of
                    True -> liftIO $ putMVar push Nothing
                    False -> do
                      v <- encodeUtf8 . fromMaybe "" <$> (liftJSM $ fromJSVal =<< chunk ! ("value" :: Text))
                      liftIO $ putMVar push (Just v)
                      go
                _ -> error "wrong number of arguments to rdrHandler"
            _ <- liftJSM $ rdrPromise # ("then" :: Text) $ [rdrHandler]
            return ()
          let out :: forall b. (S.StepT IO BS.ByteString -> IO b) -> IO b 
              out handler' = handler' .  S.Effect . fix $ \go -> do
                next <- takeMVar push
                case next of
                  Nothing -> return S.Stop
                  Just x -> return $ S.Yield x (S.Effect go)
          liftIO . putMVar result . Response status hdrs ver $ S.SourceT @IO out
        _ -> error "wrong number of arguments to Promise.then() callback"
    liftJSM $ fetchPromise # "then" $ [fetchPromiseHandler]
    liftJSM . handler =<< liftIO (takeMVar result)
