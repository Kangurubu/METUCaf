{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Text.HTML.Scalpel
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Data.Text (pack)
import Data.List (isInfixOf)
import Data.Maybe
import Monomer
import TextShow
import Data.Time

import qualified Monomer.Lens as L

data AppModel = AppModel {
  _imageList :: [Text],
  _foodList :: [Text]
} deriving (Eq, Show)

data AppEvent
  = AppInit
  deriving (Eq, Show)

makeLenses 'AppModel

listFoods :: URL -> IO (Maybe [String])
listFoods url = scrapeURL url comment
            where
            comment = chroots ("header") (text $ "h2")
            
listImages :: URL -> IO (Maybe [String])
listImages url =  do
                  images <- (scrapeURL url (attrs "src" ("img" @: [hasClass "img-responsive"])))
                  return images

  
buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where

  foodWid foodImage foodTitle = case foodTitle of
                                  [] -> vstack [ label "Seçili tarihe ait yemek bulunamadı." `styleBasic` [textFont "Regular", textSize 20, padding 5, bgColor sectionBg] ]
                                  otherwise -> hstack_ [childSpacing_ 32] (map renderFood (zip foodImage foodTitle)) 

  renderFood (fI,fT) =vstack [
                                box  $ image fI `styleBasic` [height 200, width 200, borderB 2 white],
                                spacer,
                                label fT `styleBasic` [textFont "Regular", textSize 16, textCenter, padding 5, textColor black]
                                ] `styleBasic` [textFont "Regular", bgColor sectionBg, border 3 white, radius 5]

  header = label caption `styleBasic` styles where
    caption = "Bugün Yemekhanede"
    styles = [textFont "Bold", textSize 40, padding 10, bgColor sectionBg, textCenter, border 3 white]

  subheader caption = label caption `styleBasic` styles where
    styles = [textFont "Bold", textSize 20, padding 10, bgColor sectionBg, textLeft, border 3 white]

  mainColor = rgbHex "454545"
  sectionBg = rgbHex "F4EEE0"

  widgetTree = vstack [
      header,
      spacer,
      subheader "Öğle Yemeği",
      spacer,
      foodWid (fst (splitAt 4 (model ^. imageList))) (fst (splitAt 4 (model ^. foodList))),
      spacer,
      subheader "Akşam Yemeği",
      spacer,
      foodWid (snd (splitAt 4 (model ^. imageList))) (snd (splitAt 4 (model ^. foodList)))
    ] `styleBasic` [textFont "Regular", padding 10, bgColor mainColor]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []

main :: IO ()
main = do
  foods <- listFoods "https://kafeterya.metu.edu.tr" 
  photos <- listImages "https://kafeterya.metu.edu.tr"
  let model = AppModel (map pack (fromJust photos)) (map pack (fromJust foods))
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowState ( MainWindowNormal (940,700)),
      appWindowTitle "METUCaf",
      appWindowIcon "./assets/images/logo.png",
      appTheme lightTheme,
      appFontDef "Regular" "./assets/fonts/cerco_regular.otf",
      appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
      appWindowResizable False,
      appInitEvent AppInit
      ]
      