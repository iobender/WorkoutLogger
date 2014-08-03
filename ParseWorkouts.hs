module ParseWorkouts where

import Data.List
import Data.Char
import Text.Read
import System.IO
import Workout

--lookup ignoring case and whitespace
lookup' :: String -> [(String, a)] -> Maybe a
lookup' = lookup . unwords . words . map toLower

--reads a line from stdin that contains 1+ alpha-numeric character
getString :: String -> IO String
getString p =
	putStr p >>
	hFlush stdout >>
	getLine >>=
	(\s -> 
		if length (filter isAlphaNum s) == 0 then getString p
		else return s)

--reads from stdin until the line is readable for the correct type
getRead :: Read a => String -> IO a
getRead p = 
	putStr p >> 
	hFlush stdout >> 
	getLine >>= 
	(\s -> case readMaybe s of
	 	Nothing -> getRead p
		Just v -> return v)

--prints the keys of an association list
putKeys :: [(String, a)] -> IO ()
putKeys m = putStrLn $ concat $ map (\(k,_) -> k ++ " ") m

--given an association table, reads stdin until the one of the keys is 
--entered, ignoring case and whitespace, and returns that value
getLookup :: String -> [(String, a)] -> IO a
getLookup p m = 
	putStr p >> 
	hFlush stdout >> 
	getLine >>=
	(\s -> if s == "..." then putKeys m >> getLookup p m else
		case lookup' s m of
	 		Nothing -> getLookup p m 
			Just v -> return v)

--gets an entire workout from stdin. Main function to be called
getWorkout :: IO Workout
getWorkout = do
	wt <- getWorkoutTypeFunc >>= (\f -> f >>= return)
	com <- getCommon
	return $ Workout wt com

--the below are helper functions to aid in getting a Workout

getWorkoutTypeFunc :: IO (IO WorkoutType)
getWorkoutTypeFunc = getLookup "type: " [("distance", getDistanceWorkout), ("core", getCoreWorkout), ("weights", getWeightsWorkout), ("sports", getSportsWorkout)]	       

getDistanceWorkout = do
	dw <- getLookup "dist type: " [("run", Run), ("bike", Bike), ("swim", Swim)]
	distance <- getRead "distance: "
	time <- getRead "time: "
	return $ Distance dw distance time

getCoreWorkout = do
	cw <- getLookup "core type: " [("pushups", Pushups), ("crunches", Crunches), ("sidedips", SideDips)]
	coreReps <- getRead "reps: "
	return $ Core cw coreReps

getWeightsWorkout = do
	ww <- getLookup "weights type: " [("curls", Curls), ("bench", Bench)]
	weight <- getRead "weight: "
	weightReps <- getRead "reps: "
	return $ Weights ww weight weightReps

getSportsWorkout = do
	sw <- getLookup "sports type: " [("baseball", Baseball), ("soccer", Soccer), ("frisbee", Frisbee), ("football", Football)]
	duration <- getRead "duration: "
	return $ Sports sw duration

getCommon :: IO Common
getCommon = do
	date <- getString "date: "
	tod <- getString "time of day: "
	place <- getString "place: "
	weather <- getString "weather: "
	diff <- getDifficulty
	gear <- getString "gear: "
	return $ Common date tod place weather diff gear 


getDifficulty :: IO Difficulty
getDifficulty = getLookup "difficulty: " [("easy", Easy), ("medium", Medium), ("hard", Hard)]

