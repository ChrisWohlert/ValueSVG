{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import           Diagrams.Backend.SVG
import qualified Diagrams.Prelude as D
import           Graphics.Svg.Core (renderBS)
import           Text.Blaze (Markup, unsafeLazyByteString)
import           Text.Blaze.Renderer.Utf8 (renderMarkup)

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = 
        "users" :> Get '[JSON] [User]
  :<|>  "svg" :> Get '[SVG] Markup

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users
    :<|> return (diagramToMarkup (D.circle 1))

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]


instance Accept SVG where
  contentType _ = "image/svg+xml"

instance MimeRender SVG Markup where
  mimeRender _ = renderMarkup

diagramToMarkup :: D.Diagram B -> Markup
diagramToMarkup = unsafeLazyByteString . renderBS . D.renderDia SVG (SVGOptions (D.mkWidth 250) Nothing "" [] True)