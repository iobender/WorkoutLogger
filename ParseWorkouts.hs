module ParseWorkouts where

import Data.Char
import Workout

-- fix whitespace and make lowercase
basestr :: String -> String
basestr = unwords . words . map toLower

lookstr :: String -> [(String, a)] -> Maybe a
lookstr = lookup . basestr

difficultyToks :: [(String, Difficulty)]
difficultyToks = [("easy", Easy), ("medium", Medium), ("hard", Hard)]

getWorkout :: IO Workout
--placeholder for actual parsing of input
getWorkout = return $ Workout Easy $ Sports Frisbee 100

getDifficulty :: IO Difficulty
getDifficulty = 
	getLine >>= 
	(\s -> case lookstr s difficultyToks of
	 	Nothing -> getDifficulty
		Just d -> return d)

workoutTypeFuncToks :: [(String, IO WorkoutType)]
workoutTypeFuncToks = [("distance", getDistanceWorkout),
		      ("core", getCoreWorkout),
	      	      ("weights", getWeightsWorkout),
		      ("sports", getSportsWorkout)]	      

getWorkoutTypeFunc :: IO (IO WorkoutType)
getWorkoutTypeFunc = 
	getLine >>=
	(\s -> case lookstr s workoutTypeFuncToks of
	 	Nothing -> getWorkoutTypeFunc
		Just f -> return f)

getDistanceWorkout = error "distance not yet implemented"
getCoreWorkout = error "core not yet implemented"
getWeightsWorkout = error "weights not yet implemented"
getSportsWorkout = error "sports not yet implemented"
