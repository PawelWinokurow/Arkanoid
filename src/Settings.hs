module Settings (module Settings) where

screenWidth, screenHeight, screenOffset, fps, numberSoundThreads :: Int
screenWidth        = 1024
screenHeight       = 768
screenOffset       = 100
numberSoundThreads = 20
fps                = 60

ballRadius, paddleHeight, halfPaddleWidth, paddleWidth, buttonHeight, buttonWidth, arrowButtonHeight, arrowButtonWidth, ballFullVelocity :: Float
ballRadius        = 3
paddleHeight      = 32
halfPaddleWidth   = 70
paddleWidth       = 140
buttonWidth       = 150
buttonHeight      = 50
arrowButtonWidth  = 40
arrowButtonHeight = 20
ballFullVelocity  = 700

brickCols, brickRows :: Int
brickCols = 10
brickRows = 5

brickWidth, brickHeight, brickMarginLeft, brickMarginTop, brickMarginBetween :: Float
brickWidth         = 100
brickHeight        = 50
brickMarginLeft    = 3
brickMarginTop     = 3
brickMarginBetween = 2 

symbolHeight, symbolWidth :: Int
symbolHeight = 72
symbolWidth  = 72