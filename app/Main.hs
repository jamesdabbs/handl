{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Catch (catchAll, SomeException)
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Time
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import Network.Wai
import Network.Wai.Handler.Warp
import Servant


-- | Request-specific data
--   Reasonable additions here would include a "current user" or
--     generic per-request cache
data HandlerContext = HandlerContext
  { requestAuthHeader :: Maybe String
  , requestTimestamp  :: UTCTime
  , requestId         :: UUID.UUID
  } deriving Show

-- | Sitewide configuration
--   This is where e.g. Rollbar credentials would go
data Config = Config
  { favoriteNumber :: Int
  , errorHandler   :: Config -> HandlerContext -> SomeException -> EitherT ServantErr IO ()
  }


-- | Our application monads: `Handlers` sitting on top of `Actions`
--   `Handlers` have stateful access to a HandlerContext object (and so would be
--     suitable for handling incoming web requests)
--   `Actions` have read access to the config (and thus, in a real application, DB access)
--     and so would be suitable for lower-level operations, background jobs, &c.
newtype Action a = Action
  { unAction :: ReaderT Config IO a
  } deriving (Functor, Applicative, Monad, MonadReader Config)

newtype Handler a = Handler
  { unHandler :: StateT HandlerContext Action a
  } deriving (Functor, Applicative, Monad, MonadReader Config)


-- | Helpers for manually running our computations in the `EitherT ServantErr IO`
--   monad that Servant expects
--
--   `runHandler` catches any runtime errors, uses the Config-set error reporter,
--   and returns a somewhat-more-informative 500 error
--
--   N.B. `runAction` does not currently catch errors, as it doesn't seem appropriate
--   for the explicitly-non-web layer to have to know about HTTP status codes
runAction :: Config -> Action a -> EitherT ServantErr IO a
runAction conf a = lift $ runReaderT (unAction a) conf

runHandler :: Config -> HandlerContext -> Handler a -> EitherT ServantErr IO a
runHandler conf ctx h = catchAll go alert
  where
    go = runAction conf $ evalStateT (unHandler h) ctx

    alert :: SomeException -> EitherT ServantErr IO a
    alert err = do
      errorHandler conf conf ctx err
      left err500 { errBody = encode $ object [ "error" .= show err ] }

-- | A datatype for signaling our extra context
--   See https://haskell-servant.github.io/extending.html for more information
--     about writing custom Servant combinators
data WithHandlerContext

instance HasServer a => HasServer (WithHandlerContext :> a) where
  -- Handlers have the extra context injected as the first argument to their handlers
  type ServerT (WithHandlerContext :> a) m = HandlerContext -> ServerT a m

  route Proxy sub req res = do
    requestTimestamp <- getCurrentTime
    requestId        <- UUID.nextRandom
    let requestAuthHeader = BS.unpack <$> lookup "Authorization" (requestHeaders req)
        ctx = HandlerContext{..}
    route (Proxy :: Proxy a) (sub ctx) req res


-- | Our (intended) Servant API type
type Api = "config"  :> Get '[JSON] Int
      :<|> "request" :> Get '[JSON] String
      :<|> "error"   :> Get '[JSON] Int

-- | Our (actual) Servant API type - _all_ handlers get extra context injected
type XApi = WithHandlerContext :> Api



-- | A few example monadic actions at both layers
getAuth :: Handler (Maybe String)
getAuth = Handler $ gets requestAuthHeader

failingAction :: Action Int
failingAction = error "This action has a runtime error"

aToH :: Action a -> Handler a
aToH = Handler . lift


-- | Actual server implementation
--   Note that we work throughout in the Handler monad, and don't have to
--   worry about passing HandlerContext (though we can access it when needed)
baseServerT :: ServerT Api Handler
baseServerT = fromConfig :<|> fromRequest :<|> failing
  where
    fromConfig :: Handler Int
    fromConfig = asks favoriteNumber

    fromRequest :: Handler String
    fromRequest = fromMaybe "No `Authorization` header set" <$> getAuth

    failing :: Handler Int
    failing = aToH failingAction


-- | The magic part about threading data through
--   See http://haskell-servant.github.io/tutorial/server.html#using-another-monad-for-your-handlers
--   for more information on `enter`
mkServer :: ServerT Api Handler -> Config -> HandlerContext -> Server Api
mkServer spec conf ctx = enter (Nat $ runHandler conf ctx) spec

main :: IO ()
main = run 3333 . serve (Proxy :: Proxy XApi) $ mkServer baseServerT conf
  where
    conf :: Config
    conf = Config
      { favoriteNumber = 42
      , errorHandler = logError
      }

    -- | Top-level error monitoring
    --   In reality, this would read e.g. Rollbar credentials from Config and
    --   post the error to Rollbar, along with User data from the HandlerContext
    logError :: Config -> HandlerContext -> SomeException -> EitherT ServantErr IO ()
    logError _ ctx err = liftIO $ do
      putStrLn $ "Caught error: " ++ (show err)
      putStrLn $ "Handler context: " ++ (show ctx)
