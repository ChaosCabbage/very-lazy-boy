module Main where

import Control.Monad (when, unless)
import qualified Graphics.UI.GLFW          as GLFW

unless' :: Monad m => m Bool -> m () -> m ()
unless' action falseAction = do
    b <- action
    unless b falseAction

run :: GLFW.Window -> IO ()
run win = unless' (GLFW.windowShouldClose win) $ do
    GLFW.pollEvents
    run win

main :: IO ()
main = do
    let width  = 640
        height = 480

    withWindow width height "GLFW-b-demo" $ \win -> do    
        GLFW.swapInterval 1
        run win

    

--------------------------------------------------------------------------------

-- GLFW-b is made to be very close to the C API, so creating a window is pretty
-- clunky by Haskell standards. A higher-level API would have some function
-- like withWindow.

-- This function is copied shamelessly from https://github.com/bsl/GLFW-b-demo

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]
