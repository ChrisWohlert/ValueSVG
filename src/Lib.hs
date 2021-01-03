{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}


module Lib
    ( startApp
    , app
    ) where

import           Codec.Picture
import           Data.Aeson
import           Data.Aeson.TH
import           Diagrams.Backend.SVG
import qualified Diagrams.Prelude         as D
import           Graphics.Svg.Core        (renderBS)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Text.Blaze               (Markup, unsafeLazyByteString)
import           Text.Blaze.Renderer.Utf8 (renderMarkup)
import           ValueSVG

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
    :<|> return (diagramToMarkup $ percentageCircle (Percentage 40))

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

instance Accept SVG where
  contentType _ = "image/png"

instance MimeRender SVG Markup where
  mimeRender _ = renderMarkup

diagramToMarkup :: D.Diagram B -> Markup
diagramToMarkup = unsafeLazyByteString . renderBS . D.renderDia SVG (SVGOptions (D.mkWidth 250) Nothing "" [] True)
