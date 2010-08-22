
module Main where

--import Qtc

import Qtc.Classes.Base
-- Qtc.close
import Qtc.Classes.Qccs
import Qtc.Classes.Core
import Qtc.Classes.Gui
import Qtc.ClassTypes.Gui
import Qtc.ClassTypes.Core
import Qtc.Core.Base
import Qtc.Gui.Base
import Qtc.Core.QCoreApplication
import Qtc.Gui.QApplication
--import Qtc.Gui.QWidget
import Qtc.Gui.QPushButton
-- qUiLoader
import Qtc.Tools.QUiLoader
-- qFile
import Qtc.Core.QFile
-- qVariant
import Qtc.Core.QVariant
-- fReadOnly
import Qtc.Enums.Core.QIODevice


import Qtc.Classes.Base
import Qtc.Classes.Qccs
import Qtc.Classes.Qccs_h
import Qtc.Classes.Core
import Qtc.Classes.Gui
import Qtc.Classes.Network
import Qtc.Enums.Base
import Qtc.ClassTypes.Core
import Qtc.Core.Base
import Qtc.Enums.Core.Qt
import Qtc.Core.QSize
import Qtc.Core.QCoreApplication
import Qtc.Core.QIODevice
import Qtc.Enums.Core.QIODevice
import Qtc.Core.QFile
import Qtc.ClassTypes.Gui
import Qtc.Gui.Base
import Qtc.Gui.QApplication
import Qtc.Gui.QWidget
import Qtc.Gui.QColor
import Qtc.Gui.QSpinBox
import Qtc.Gui.QTextEdit
import Qtc.Gui.QLineEdit
import Qtc.Gui.QPushButton
import Qtc.Gui.QPushButton_h
import Qtc.ClassTypes.Network
import Qtc.Network.QHostAddress
import Qtc.Network.QAbstractSocket


-- stop clash with Qt's close
import qualified Network.HTTP as HTTP
import Text.JSON
import Network.URL
-- Windows
import qualified Text.XML.Light as XLT

import Debug.Trace

showt x = traceShow x x


type MyQPushButton = QPushButtonSc (CMyQPushButton)
data CMyQPushButton = CMyQPushButton

myQPushButton :: String -> IO (MyQPushButton)
myQPushButton b = qSubClass $ qPushButton b


data GoogleResponse = GoogleResponse
  {
    gstatus :: String,
    results :: [Results]
  } deriving Show

data Results = Results
  {
    types :: [String],
    formattedAddress :: String,
    addressComponents :: [JSObject JSValue],
    ggeometry :: Geometry 
  } deriving Show

data Geometry = Geometry
  {
    location :: Location
  } deriving Show

data Location = Location
  {
    lat :: Double,
    lng :: Double
  } deriving Show

toGoogleResponse obj = GoogleResponse
  {
    gstatus = getString obj "status",
    results = map toResults $ getArray ( \ (JSObject x) -> x) obj "results"
  }

toLocation obj = Location
  {
    lat = getDouble obj "lat",
    lng = getDouble obj "lng"

  }

toGeometry obj = Geometry
  {
    location = toLocation $ getObject obj "location"
  }

toResults obj = Results
  {
    types = getArray (\ (JSString x) -> fromJSString x) obj "types",
    formattedAddress = getString obj "formatted_address",
    addressComponents =  getArray ( \ (JSObject x) -> x) obj "address_components",
    ggeometry = toGeometry $ getObject obj "geometry"
  }

      

getObject obj name = let Ok (JSObject inner) = traceShow name $ valFromObj name obj in inner

getArray func obj name = let Ok (JSArray x) = valFromObj name obj in map func x

getString obj name = let Ok (JSString txt) = valFromObj name obj in fromJSString txt 

getDouble obj name = let Ok (JSRational _ num) = valFromObj name obj in fromRational num


mineCoords = location . ggeometry . head . results 
mineAddr   = formattedAddress . head . results





main :: IO ()
main
  = do
    app <- qApplication  () 
    rok <- registerResource "marble.rcc"
    loader <- qUiLoader ()
    uiFile <- qFile ":/marble.ui"
    open uiFile fReadOnly
    ui <- load loader uiFile
    close uiFile ()
        
    ui_map    <- findChild ui ("<QWidget*>", "marbleWidget")
    ui_button <- (qSubClass $ findChild ui ("<QPushButton*>", "pushButton")) :: IO (MyQPushButton)
    ui_tb     <- findChild ui ("<QLineEdit*>", "lineEdit") 
     
    connectSlot ui_button "clicked()" ui_button "click()" $ on_button_clicked ui_tb ui_map
    

    qshow ui ()
    ok <- qApplicationExec ()
    return ()


on_button_clicked :: QLineEdit () ->  QWidget () -> MyQPushButton -> IO ()
on_button_clicked tb mp this
  = do
    text  <- text tb () 
    --print text

    googleLocation <- getXml text --getLocation text

    case ( getMaybeElem googleLocation "lat", getMaybeElem googleLocation "lng" ) of
       (Just lat, Just lng) -> setNewLocation mp lat lng
       (_,_) -> return ()

    return ()
    


setNewLocation mp lat lng
  = do    
    qtLat <- qVariant (getElemContents lat :: Double)
    qtLng <- qVariant (getElemContents lng :: Double)
    
    latOk <- qObjectSetProperty mp "latitude" qtLat
    lngOk <- qObjectSetProperty mp "longitude" qtLng
    return ()    
  
  
getLocation place = (HTTP.simpleHTTP $ HTTP.getRequest url) >>= HTTP.getResponseBody >>= (return . toGoogleResponse . tolist)
    where
        url = showt $ "http://maps.google.com/maps/api/geocode/json?address=" ++ encString False (\_ -> False) place ++ "&sensor=false"
        tolist str = let Ok x = decode $ showt str
                     in x

getXml place = (HTTP.simpleHTTP $ HTTP.getRequest url) >>= HTTP.getResponseBody >>= (return . toElem)
    where
        url = "http://maps.google.com/maps/api/geocode/xml?address=" ++ encString False (\_ -> False) place ++"London&sensor=false"
        toElem str = let Just x = XLT.parseXMLDoc str in x


getDoubleElem xmlDoc strElem = let Just elem = getMaybeElem xmlDoc strElem  
                                in (getElemContents elem) :: Double 


getMaybeElem xmlDoc strElem = XLT.findElement (XLT.unqual strElem) xmlDoc

getElemContents :: Read a => XLT.Element -> a
getElemContents = read . XLT.strContent