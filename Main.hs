{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Main where



import FndQueens
import Control.Monad
import Control.Monad.State.Lazy
import Data.Traversable
import Data.IORef
import Data.List hiding (lookup)
import Data.String
import System.Directory
import System.FilePath.Posix
import System.Environment
import Data.Map.Strict as M hiding (map, null)
import Data.Text (pack) 
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe (fromJust)
import Data.GI.Base.ManagedPtr
import Data.GI.Base
import GI.Gtk.Objects as GIO
import qualified GI.Gtk as Gtk (main, init)
import Control.Monad.STM
import Data.GI.Gtk.Threading
import Paths_queens8
import GI.Gtk
        (widgetShowAll, mainQuit, onWidgetDestroy, onButtonClicked, Button(..),
        Window(..), builderGetObject, builderAddFromFile, builderNew, Fixed, ButtonsType(..), MessageDialog, dialogRun)




queenState = do
        t <- Control.Monad.State.Lazy.get
        put $ tail t
        return $ head t


listW = [(i,j) | i<-[1..8], j<-[1..8]]


getfromlistw         :: Builder  -> (Int,Int) ->  IO GIO.Image
getfromlistw b (i,j) =  builderGetObject b (pack ("image"++show i ++ show j)) >>= unsafeCastTo GIO.Image . fromJust
makeMapLQ l          =  fromList (zip listW l)
applyMapLQ m l pn    =  mapM_ (flip imageSetFromFile $ Just pn) $ map (m !) l 



main :: IO ()
main = do
  dirMy       <-  liftM dropFileName getExecutablePath 
  let gladeNm =   pack $ dirMy </> "queens.glade"
  let picName =   dirMy </> "ch1.png"



  Gtk.init        Nothing
  builder     <-  builderNew
  st<-getDataFileName "queens.glade"
  builderAddFromFile builder $ gladeNm -- (pack st)
  mainWindow  <-  builderGetObject builder "mainWindow" >>= unsafeCastTo Window . fromJust
  button      <-  builderGetObject builder "nextb" >>= unsafeCastTo Button . fromJust




  --delete and paint queens
  imgLS        <- traverse (getfromlistw builder) listW
  let mapOfImg =  makeMapLQ imgLS
  mapM_           imageClear $ elems mapOfImg



  listRef      <- newIORef [] 
  cntRef       <- newIORef 90
  forkIO $ do
       postGUIASync $ buttonSetLabel button "Wait..."
       a <-fqWorker
       fq<-atomically $ do 
           fqq<-readTVar a
           check (length fqq >=92)
           return $ findQueens fqq
       let (ls, qs) = runState queenState fq 
       ls `seq` writeIORef  listRef qs
       postGUIASync $ applyMapLQ mapOfImg ls picName
       postGUIASync $ buttonSetLabel button "Next 91"



  --handler BUTTON "NEXT"
  onButtonClicked button $ readIORef cntRef >>= (\cnt ->
            if cnt < 0 
                then do
                    buttonSetLabel button "That is all!"
                    writeIORef cntRef $ -1
                else do 
                    qqs            <- readIORef listRef
                    let (lss, qss) =  runState queenState qqs
                    writeIORef        listRef qss
                    mapM_             imageClear (elems mapOfImg)
                    buttonSetLabel    button $ pack ("Next " ++ show cnt)
                    applyMapLQ        mapOfImg lss picName
                    writeIORef cntRef $ cnt-1)



  onWidgetDestroy  mainWindow mainQuit
  widgetShowAll    mainWindow
  Gtk.main
