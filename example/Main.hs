{-# LANGUAGE LambdaCase #-}
module Main where

import System.Console.Haskeline
import qualified System.Console.Haskeline.Brick as HB

import Brick
import Brick.BChan
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Graphics.Vty as V

import Control.Monad (void)
import Control.Concurrent (forkFinally)
import Control.Monad.IO.Class (liftIO)

data Event = FromHBWidget HB.ToBrick | HaskelineDied (Either SomeException ())

data Name = TheApp | HaskelineWidget
    deriving (Ord, Eq, Show)

data MyState = MyState { haskelineWidget :: HB.Widget Name }

app :: HB.Config Event -> App MyState Event Name
app c = App { appDraw = drawUI
            , appChooseCursor = const $ showCursorNamed HaskelineWidget
            , appHandleEvent = handleEvent c
            , appStartEvent = return
            , appAttrMap = const $ attrMap V.defAttr []
            }

handleEvent :: HB.Config Event
            -> MyState -> BrickEvent Name Event -> EventM Name (Next MyState)
handleEvent c s @ MyState{haskelineWidget = hw} e = do
    hw' <- HB.handleEvent c hw e
    handleAppEvent (s { haskelineWidget = hw' }) e

handleAppEvent :: MyState -> BrickEvent Name Event -> EventM Name (Next MyState)
handleAppEvent s (AppEvent (HaskelineDied e)) = halt s
handleAppEvent s (VtyEvent (V.EvKey V.KEsc [])) = halt s
handleAppEvent s _ = continue s

drawUI :: MyState -> [Widget Name]
drawUI s = [ (vLimit 4 $ top) <=> bottom ]
    where
        top = C.center $ str "yo"
        bottom = B.border $ HB.render (haskelineWidget s)

runHaskeline :: HB.Config Event -> IO ()
runHaskeline c = runInputTBehavior (HB.useBrick c) defaultSettings loop
   where
       loop = do
           minput <- getInputLine "% "
           case minput of
             Nothing -> return ()
             Just input -> do
                 outputStr input
                 loop

main :: IO ()
main = do
    chan <- newBChan 10
    config <- HB.configure
            chan
            FromHBWidget
            (\case { FromHBWidget x -> Just x; _ -> Nothing })
    _ <- forkFinally
            (runHaskeline config)
            (writeBChan chan . HaskelineDied)
    void $ customMain
        (V.mkVty V.defaultConfig)
        (Just chan)
        (app config)
        MyState { haskelineWidget = HB.initialWidget HaskelineWidget }
