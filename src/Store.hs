module Store (module Store) where

import Control.Monad
import Data.List.Split

loadHighScores :: IO [(String, String)]
loadHighScores = do  
  contents <- readFile "highscore.txt"  
  let scoreLines = lines contents 
  let scorePairs = map ((\[a,b] -> (a,b)) . splitOn " ") scoreLines
  return scorePairs     
  
saveHighScore ::  [(String, String)] -> IO ()
saveHighScore scores = do     
  writeFile "highscore.txt" $ unlines $ map (\pair -> fst pair ++ " " ++ snd pair) scores