module ParseWorkouts where

import Data.Char
import Text.Read
import System.IO
import Workout

--lookup ignoring case and whitespace
lookup' :: String -> [(String, a)] -> Maybe a
lookup' = lookup . unwords . words . map toLower

getRead :: Read a => String -> IO a
getRead p = 
	putStr p >> 
	hFlush stdout >> 
	getLine >>= 
	(\s -> case readMaybe s of
	 	Nothing -> getRead p
		Just v -> return v)

--given an association table, reads stdin until the one of the keys is 
--entered, ignoring case and whitespace, and returns that value
getLookup :: String -> [(String, a)] -> IO a
getLookup p m = 
	putStr p >> 
	hFlush stdout >> 
	getLine >>=
	(\s -> case lookup' s m of
	 	Nothing -> getLookup p m 
		Just v -> return v)

--gets an entire workout from stdin. Main function to be called
getWorkout :: IO Workout
getWorkout = do
	diff <- getDifficulty
	wt <- getWorkoutTypeFunc >>= (\f -> f >>= return)
	return $ Workout diff wt

getDifficulty :: IO Difficulty
getDifficulty = getLookup "Diff: " [("easy", Easy), ("medium", Medium), ("hard", Hard)]

getWorkoutTypeFunc :: IO (IO WorkoutType)
getWorkoutTypeFunc = getLookup "type: " [("distance", getDistanceWorkout), ("core", getCoreWorkout), ("weights", getWeightsWorkout), ("sports", getSportsWorkout)]	       

getDistanceWorkout = do
	dwt <- getLookup "dist type: " [("run", Run), ("bike", Bike), ("swim", Swim)]
	distance <- getRead "distance: "
	time <- getRead "time: "
	return $ Distance dwt distance time

getCoreWorkout = do
	cwt <- getLookup "core type: " [("pushups", Pushups), ("crunches", Crunches), ("sidedips", SideDips)]
	coreReps <- getRead "reps: "
	return $ Core cwt coreReps

getWeightsWorkout = do
	wwt <- getLookup "weights type: " [("curls", Curls), ("bench", Bench)]
	weight <- getRead "weight: "
	weightReps <- getRead "weightReps"
	return $ Weights wwt weight weightReps

getSportsWorkout = do
	swt <- getLookup "sports type: " [("baseball", Baseball), ("soccer", Soccer), ("frisbee", Frisbee), ("football", Football)]
	duration <- getRead "duration: "
	return $ Sports swt duration
