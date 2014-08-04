-- Matthew Bender
--
-- describes various types of workouts
module Workout where

import Data.List
import System.Locale
import Data.Time
import Data.Time.Format

data Workout = Workout WorkoutType Common deriving (Eq, Show)

data WorkoutType = 
	Distance { 
		dw :: DistanceWorkout,
		distance :: Float,
		time :: Int
	} | 
	Core {
		cw :: CoreWorkout,
		coreReps :: Int
	} |
	Weights {
		ww :: WeightsWorkout,
		weight :: Int,
		weightReps :: Int
	} |
	Sports {
		sw :: SportsWorkout,
		duration :: Int
	} 
	deriving (Eq, Show)

data DistanceWorkout = Run | Bike | Swim | OtherDistance String deriving (Eq, Show)
data CoreWorkout = Pushups | Crunches | SideDips | OtherCore String deriving (Eq, Show)
data WeightsWorkout = Curls | Bench | OtherWeights String deriving (Eq, Show)
data SportsWorkout = Baseball | Soccer | Frisbee | Football | OtherSports String deriving (Eq, Show)

--gets a list of Strings describing the workout type
workoutTypeToTokens :: WorkoutType -> [String]
workoutTypeToTokens w = case w of
	Distance _ _ _ -> 
		["Type: " ++ (show $ dw w)] ++ 
		["Distance: " ++ (show $ distance w)] ++ 
		["Time: " ++ (show $ time w)]
	Core _ _ -> 
		["Type: " ++ (show $ cw w)] ++
		["Reps: " ++ (show $ coreReps w)]
	Weights _ _ _ -> 
		["Type: " ++ (show $ ww w)] ++ 
		["Weight: " ++ (show $ weight w)] ++ 
		["Reps: " ++ (show $ weightReps w)]
	Sports _ _ ->
		["Type: " ++ (show $ sw w)] ++
		["Duration: " ++ (show $ duration w)] 

--String representation of this Workout type, one token per line
workoutTypeToString :: WorkoutType -> String
workoutTypeToString = unlines . workoutTypeToTokens

--tokens joined together on one line, split by |
workoutTypeToLine :: WorkoutType -> String
workoutTypeToLine = intercalate "|" . workoutTypeToTokens

workoutToString :: Workout -> String
workoutToString (Workout w c) = "Workout:\n" ++ indent (workoutTypeToString w ++ commonToString c)

workoutToLine :: Workout -> String
workoutToLine (Workout w c) = "Workout|" ++ workoutTypeToLine w ++  "|" ++ commonToLine c

indent :: String -> String
indent = unlines . map ("\t" ++) . lines

data Common = Common {
	datetime :: UTCTime,
	place :: String,
	weather :: String,
	diff :: Difficulty,
	gear :: String
} deriving (Eq, Show)

data Difficulty = Easy | Medium | Hard deriving (Eq, Show)

commonToTokens :: Common -> [String]
commonToTokens (Common datetime place weather diff gear) = 
	["Date: " ++ formatTime defaultTimeLocale "%-m/%-d/%Y" datetime] ++
	["time: " ++ formatTime defaultTimeLocale "%-I:%M %p" datetime] ++ 
	["Place: " ++ place] ++
	["Weather: " ++ weather] ++
	["Difficulty: " ++ show diff] ++
	["Gear: " ++ gear] 

commonToString :: Common -> String
commonToString = unlines . commonToTokens

commonToLine :: Common -> String
commonToLine = intercalate "|" . commonToTokens
