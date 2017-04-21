-- Somewhere to start playing with graphical applications.
-- Obviously in time I would like graphics for my gameboy.

module Main(main) where

import Graphics.Gloss

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = circle 80

main :: IO ()
main = display window background drawing
