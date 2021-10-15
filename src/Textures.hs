{-# LANGUAGE TemplateHaskell #-}

module Textures (module Textures) where

import Graphics.Gloss
import Control.Lens

data Textures = Textures 
  { _backgroundGameTex         :: Picture
  , _paddleTex                 :: Picture
  , _ballTex                   :: Picture
  , _backgroundMainTex         :: Picture
  , _newGameButtonTex          :: Picture
  , _newGameButtonOnHoverTex   :: Picture
  , _highScoreButtonTex        :: Picture
  , _highScoreButtonOnHoverTex :: Picture
  , _backgroundGameOverTex     :: Picture
  , _replayButtonTex           :: Picture
  , _replayButtonOnHoverTex    :: Picture
  , _saveButtonTex             :: Picture
  , _saveButtonOnHoverTex      :: Picture
  , _mainMenuButtonTex         :: Picture
  , _mainMenuButtonOnHoverTex  :: Picture
  , _exitGameButtonTex         :: Picture
  , _exitGameButtonOnHoverTex  :: Picture
  , _backButtonTex             :: Picture
  , _backButtonOnHoverTex      :: Picture
  , _backgroundHighScoreTex    :: Picture
  , _arrowUpTex                :: Picture
  , _arrowDownTex              :: Picture
  , _arrowUpOnHoverTex         :: Picture
  , _arrowDownOnHoverTex       :: Picture
  , _numsTex                   :: [(Char, Picture)]
  , _symbolsTex                :: [(Char, Picture)]
  }
makeLenses ''Textures

loadTextures :: IO Textures
loadTextures = do
  backgroundGameImage         <- loadBMP "textures/game/background.bmp"
  paddleImage                 <- loadBMP "textures/game/paddle.bmp"
  ballImage                   <- loadBMP "textures/game/ball.bmp"
  backgroundMainImage         <- loadBMP "textures/main/background_main.bmp"
  newGameButtonImage          <- loadBMP "textures/main/new_game_button.bmp"
  newGameButtonOnHoverImage   <- loadBMP "textures/main/new_game_button_on_hover.bmp"
  highScoreButtonImage        <- loadBMP "textures/main/highscore_button.bmp"
  highScoreButtonOnHoverImage <- loadBMP "textures/main/highscore_button_on_hover.bmp"
  exitGameButtonImage         <- loadBMP "textures/main/exit_game_button.bmp"
  exitGameButtonOnHoverImage  <- loadBMP "textures/main/exit_game_button_on_hover.bmp"
  backgroundGameOverImage     <- loadBMP "textures/gameover/background_gameover.bmp"
  replayButtonImage           <- loadBMP "textures/gameover/replay_button.bmp"
  replayButtonOnHoverImage    <- loadBMP "textures/gameover/replay_button_on_hover.bmp"
  saveButtonImage             <- loadBMP "textures/gameover/save_button.bmp"
  saveButtonOnHoverImage      <- loadBMP "textures/gameover/save_button_on_hover.bmp"
  mainMenuButtonImage         <- loadBMP "textures/gameover/main_menu_button.bmp"
  mainMenuButtonOnHoverImage  <- loadBMP "textures/gameover/main_menu_button_on_hover.bmp"
  arrowUpImage                <- loadBMP "textures/gameover/up.bmp"
  arrowDownImage              <- loadBMP "textures/gameover/down.bmp"
  arrowUpOnHoverImage         <- loadBMP "textures/gameover/up_on_hover.bmp"
  arrowDownOnHoverImage       <- loadBMP "textures/gameover/down_on_hover.bmp"
  backButtonImage             <- loadBMP "textures/highscores/back_button.bmp"
  backButtonOnHoverImage      <- loadBMP "textures/highscores/back_button_on_hover.bmp"
  backgroundHighScoreImage    <- loadBMP "textures/highscores/background_highscores.bmp"
  numsImages                  <- loadNumbers
  alphaImages                 <- loadAlphabet

  let textures = Textures { _backgroundGameTex         = backgroundGameImage 
                          , _paddleTex                 = paddleImage
                          , _ballTex                   = ballImage
                          , _backgroundMainTex         = backgroundMainImage 
                          , _newGameButtonTex          = newGameButtonImage
                          , _newGameButtonOnHoverTex   = newGameButtonOnHoverImage
                          , _highScoreButtonTex        = highScoreButtonImage
                          , _highScoreButtonOnHoverTex = highScoreButtonOnHoverImage
                          , _backgroundGameOverTex     = backgroundGameOverImage
                          , _replayButtonTex           = replayButtonImage
                          , _replayButtonOnHoverTex    = replayButtonOnHoverImage
                          , _saveButtonTex             = saveButtonImage
                          , _saveButtonOnHoverTex      = saveButtonOnHoverImage
                          , _mainMenuButtonTex         = mainMenuButtonImage
                          , _mainMenuButtonOnHoverTex  = mainMenuButtonOnHoverImage
                          , _exitGameButtonTex         = exitGameButtonImage
                          , _exitGameButtonOnHoverTex  = exitGameButtonOnHoverImage
                          , _backButtonTex             = backButtonImage
                          , _backButtonOnHoverTex      = backButtonOnHoverImage
                          , _backgroundHighScoreTex    = backgroundHighScoreImage
                          , _numsTex                   = numsImages
                          , _symbolsTex                = alphaImages
                          , _arrowUpOnHoverTex         = arrowUpOnHoverImage 
                          , _arrowDownOnHoverTex       = arrowDownOnHoverImage 
                          , _arrowUpTex                = arrowUpImage 
                          , _arrowDownTex              = arrowDownImage 
                          }
  return textures

loadNumbers :: IO [(Char, Picture)]
loadNumbers = do
  nums <- sequence [ loadBMP ("textures/symbols/nums/" ++ [ch] ++ ".bmp") | ch <- ['0'..'9'] ]
  let listOfPairs = zip ['0'..'9'] nums
  return listOfPairs

loadAlphabet :: IO [(Char, Picture)]
loadAlphabet = do
  alpha <- sequence [ loadBMP ("textures/symbols/alpha/" ++ [ch] ++ ".bmp") | ch <- ['A'..'Z'] ]
  nums <- sequence [ loadBMP ("textures/symbols/alpha/" ++ [ch] ++ ".bmp") | ch <- ['0'..'9'] ]
  let listOfPairs = (zip ['a'..'z'] alpha) ++ (zip ['0'..'9'] nums)
  return listOfPairs