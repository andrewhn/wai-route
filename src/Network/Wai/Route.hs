-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Route
    ( Handler
    , route
    , routeWith404
    ) where

import Data.ByteString (ByteString)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Route.Tree
import Prelude hiding (lookup)

import qualified Data.ByteString.Lazy as L

-- | A 'Handler' is a generalized 'Application' that receives the captured
-- path parameters as its first argument.
type Handler m = [(ByteString, ByteString)]        -- ^ The captured path parameters.
               -> Request                          -- ^ The matched 'Request'.
               -> (Response -> m ResponseReceived) -- ^ The continuation.
               -> m ResponseReceived

-- | A 'NotFoundHandler' is equivalent to a handler but receives
-- no matched parameters
type NotFoundHandler m = Request
                       -> (Response -> m ResponseReceived)
                       -> m ResponseReceived

-- | Routes requests to 'Handler's according to a routing table.
-- No match defaults to empty 404 response
route :: Monad m
      => [(ByteString, Handler m)]
      -> Request
      -> (Response -> m ResponseReceived)
      -> m ResponseReceived
route rs rq k = routeWith404 rs nfh rq k
  where
    nfh _ k = k $ responseLBS status404 [] L.empty

-- | Routes requests to 'Handler's according to a routing table.
routeWith404 :: Monad m
             => [(ByteString, Handler m)]
             -> NotFoundHandler m
             -> Request
             -> (Response -> m ResponseReceived)
             -> m ResponseReceived
routeWith404 rs nfh rq k =
  case lookup (fromList rs) segs of
    Just  e -> value e (captured $ captures e) rq k
    Nothing -> nfh rq k
  where
    segs = segments (rawPathInfo rq)
