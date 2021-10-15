{-# LANGUAGE TemplateHaskell #-}

module GameOverView (module GameOverView) where

import GameView(initGameState)
import Textures
import DataObjects
import Settings
import Geometry
import System.IO  
import System.Exit
import Data.List
import Data.Function
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import Control.Lens

update :: Float -> GameState -> GameState
update time game = game 

render :: GameState  -> Picture
render game 
  | game ^. buttonsHover ^. isReplayButtonHovered   = pictures $ [ game ^. textures ^. backgroundGameOverTex , translate 0 (-70) $ game ^. textures ^. replayButtonOnHoverTex , translate 0 (-140) $ game ^. textures ^. mainMenuButtonTex ] ++ [(showNewScore game)]
  | game ^. buttonsHover ^. isMainMenuButtonHovered = pictures $ [ game ^. textures ^. backgroundGameOverTex , translate 0 (-70) $ game ^. textures ^. replayButtonTex , translate 0 (-140) $ game ^. textures ^. mainMenuButtonOnHoverTex ] ++ [(showNewScore game)]
  | otherwise                                   = pictures $ [ game ^. textures ^. backgroundGameOverTex , translate 0 (-70) $ game ^. textures ^. replayButtonTex , translate 0 (-140) $ game ^. textures ^. mainMenuButtonTex  ] ++ [(showNewScore game)]  

showNewScore :: GameState -> Picture
showNewScore game = if (read $ snd $ game ^. score ::Int) > minimum highscores
                        then
                          if game ^. buttonsHover ^. isSaveButtonHovered 
                            then
                              pictures $ [translate (-120) 100 enterName, translate 30 100 (convertStringToPicture $ snd $ game ^. score), game ^. textures ^. saveButtonOnHoverTex]
                            else
                              pictures $ [translate (-120) 100 enterName, translate 30 100 (convertStringToPicture $ snd $ game ^. score), game ^. textures ^. saveButtonTex]
                      else
                        pictures $ []
  where 
    enterName :: Picture
    enterName = pictures $ [ convertStringToPicture $ fst $ game ^. score, addArrows ]

    addArrows :: Picture
    addArrows
      | game ^. buttonsHover ^. isFirstUpButtonHovered    = pictures $ [translate (fromIntegral (col * 38)) 40    (game ^. textures ^. arrowUpTex)   | col <- [1, 2]] ++ 
                                                                       [translate (fromIntegral (col * 38)) (-40) (game ^. textures ^. arrowDownTex) | col <- [0..2]] ++
                                                                       [translate 0 40 (game ^. textures ^. arrowUpOnHoverTex)]
      | game ^. buttonsHover ^. isFirstDownButtonHovered  = pictures $ [translate (fromIntegral (col * 38)) 40    (game ^. textures ^. arrowUpTex)   | col <- [0..2]] ++ 
                                                                       [translate (fromIntegral (col * 38)) (-40) (game ^. textures ^. arrowDownTex) | col <- [1, 2]] ++
                                                                       [translate 0 (-40) (game ^. textures ^. arrowDownOnHoverTex)]
      | game ^. buttonsHover ^. isSecondUpButtonHovered   = pictures $ [translate (fromIntegral (col * 38)) 40    (game ^. textures ^. arrowUpTex)   | col <- [0, 2]] ++ 
                                                                       [translate (fromIntegral (col * 38)) (-40) (game ^. textures ^. arrowDownTex) | col <- [0..2]] ++
                                                                       [translate 38 40 (game ^. textures ^. arrowUpOnHoverTex)]
      | game ^. buttonsHover ^. isSecondDownButtonHovered = pictures $ [translate (fromIntegral (col * 38)) 40    (game ^. textures ^. arrowUpTex)   | col <- [0..2]] ++ 
                                                                       [translate (fromIntegral (col * 38)) (-40) (game ^. textures ^. arrowDownTex) | col <- [0, 2]] ++
                                                                       [translate 38 (-40) (game ^. textures ^. arrowDownOnHoverTex)]
      | game ^. buttonsHover ^. isThirdUpButtonHovered    = pictures $ [translate (fromIntegral (col * 38)) 40    (game ^. textures ^. arrowUpTex)   | col <- [0, 1]] ++ 
                                                                       [translate (fromIntegral (col * 38)) (-40) (game ^. textures ^. arrowDownTex) | col <- [0..2]] ++
                                                                       [translate 76 40 (game ^. textures ^. arrowUpOnHoverTex)]
      | game ^. buttonsHover ^. isThirdDownButtonHovered  = pictures $ [translate (fromIntegral (col * 38)) 40    (game ^. textures ^. arrowUpTex)   | col <- [0..2]] ++ 
                                                                       [translate (fromIntegral (col * 38)) (-40) (game ^. textures ^. arrowDownTex) | col <- [0, 1]] ++
                                                                       [translate 76 (-40) (game ^. textures ^. arrowDownOnHoverTex)]
      | otherwise                                         = pictures $ [translate (fromIntegral (col * 38)) 40    (game ^. textures ^. arrowUpTex)   | col <- [0..2]] ++ 
                                                                       [translate (fromIntegral (col * 38)) (-40) (game ^. textures ^. arrowDownTex) | col <- [0..2]]

    convertStringToPicture :: String -> Picture
    convertStringToPicture text = pictures $ [ translate (fromIntegral (col * 38)) 0 (snd $ (filter (\symbol -> (fst symbol) == text!!col)  (game ^. textures ^. symbolsTex))!!0) | col <- [0..length text - 1]]

    highscores :: [Int]
    highscores = map (\scorePair -> read (snd scorePair) ::Int) (game ^. highScores ^. scores)


handleInput :: Event -> GameState -> GameState
handleInput (EventMotion (x, y)) game = mouseMotionHandler game (x,y)
handleInput (EventKey (MouseButton LeftButton) Down _ (x, y)) game = mouseClickHandler game (x,y)
handleInput _ game = game

mouseClickHandler :: GameState -> Position -> GameState
mouseClickHandler game mousePosition
  | game ^. buttonsHover ^. isFirstUpButtonHovered    = set score ([fst $ (game ^. textures ^. symbolsTex)!!(nextIndex 0)] ++ slice 1 2 (fst $ game ^. score), snd $ game ^. score) game
  | game ^. buttonsHover ^. isFirstDownButtonHovered  = set score ([fst $ (game ^. textures ^. symbolsTex)!!(prevIndex 0)] ++ slice 1 2 (fst $ game ^. score), snd $ game ^. score) game
  | game ^. buttonsHover ^. isSecondUpButtonHovered   = set score ([(fst $ (game ^. score))!!0] ++ [fst $ (game ^. textures ^. symbolsTex)!!(nextIndex 1)] ++ [(fst $ game ^. score)!!2], snd $ game ^. score) game
  | game ^. buttonsHover ^. isSecondDownButtonHovered = set score ([(fst $ (game ^. score))!!0] ++ [fst $ (game ^. textures ^. symbolsTex)!!(prevIndex 1)] ++ [(fst $ game ^. score)!!2], snd $ game ^. score) game
  | game ^. buttonsHover ^. isThirdUpButtonHovered    = set score (slice 0 1 (fst $ game ^. score) ++ [fst $ (game ^. textures ^. symbolsTex)!!(nextIndex 2)], snd $ game ^. score) game
  | game ^. buttonsHover ^. isThirdDownButtonHovered  = set score (slice 0 1 (fst $ game ^. score) ++ [fst $ (game ^. textures ^. symbolsTex)!!(prevIndex 2)], snd $ game ^. score) game
  | game ^. buttonsHover ^. isReplayButtonHovered     = startGame game
  | game ^. buttonsHover ^. isMainMenuButtonHovered   = set currentView MainMenu game
  | game ^. buttonsHover ^. isSaveButtonHovered       = addScore
  | otherwise                                         = game  
  where
    nextIndex :: Int -> Int
    nextIndex i = ((fromJust $ findIndex (\pair -> (fst pair) == (fst $ game ^. score)!!i) (game ^. textures ^. symbolsTex)) + 1) `mod` 36

    prevIndex :: Int -> Int
    prevIndex i = ((fromJust $ findIndex (\pair -> (fst pair) == (fst $ game ^. score)!!i) (game ^. textures ^. symbolsTex)) - 1) `mod` 36

    fromJust :: Maybe a -> a
    fromJust (Just a) = a

    addScore :: GameState
    addScore =  game {_currentView = MainMenu, _highScores = (game ^. highScores) {_save = True, _scores = take 5 $ reverse $ sortBy (\ (_, x) (_, y) -> compare (read x ::Int) (read y ::Int)) ((game ^. highScores ^. scores) ++ [game ^. score])}}

    slice :: Int -> Int -> [a] -> [a]
    slice from to xs = take (to - from + 1) (drop from xs)

startGame :: GameState -> GameState
startGame game = GameView.initGameState game

mouseMotionHandler :: GameState -> Position -> GameState
mouseMotionHandler game mousePosition = game {_buttonsHover = (_buttonsHover game) { _isReplayButtonHovered     = mousePosition `insideButton`      (0, (-70))
                                                                                   , _isMainMenuButtonHovered   = mousePosition `insideButton`      (0, (-140))
                                                                                   , _isSaveButtonHovered       = mousePosition `insideButton`      (0,0) 
                                                                                   , _isFirstUpButtonHovered    = mousePosition `insideArrowButton` ((-120), 140)
                                                                                   , _isFirstDownButtonHovered  = mousePosition `insideArrowButton` ((-120), 60)
                                                                                   , _isSecondUpButtonHovered   = mousePosition `insideArrowButton` ((-82), 140)
                                                                                   , _isSecondDownButtonHovered = mousePosition `insideArrowButton` ((-82), 60) 
                                                                                   , _isThirdUpButtonHovered    = mousePosition `insideArrowButton` ((-44), 140)
                                                                                   , _isThirdDownButtonHovered  = mousePosition `insideArrowButton` ((-44), 60)
                                                                                   }                                                                                  
                                             }