module ParseWorkouts where

import Data.Char
import Workout

-- fix whitespace and make lowercase
basestr :: String -> String
basestr = unwords . words . map toLower

lookstr :: String -> [(String, a)] -> Maybe a
lookstr = lookup . basestr

getWorkout :: IO Workout
--placeholder for actual parsing of input
getWorkout = return $ Workout Easy $ Sports Frisbee 100

getDifficulty :: IO Difficulty
getDifficulty = 
	getLine >>= 
	(\s -> case lookstr s difficultyStrMap of
	 	Nothing -> getDifficulty
		Just d -> return d)

