-- Matthew Bender
--
-- describes various types of workouts
module Workout where

import Data.List

type Time = Int

data Workout = Workout Difficulty WorkoutType deriving (Eq, Show)

data Difficulty = Easy | Medium | Hard deriving (Eq, Show)

data WorkoutType = 
	Distance { 
		dw :: DistanceWorkout,
		distance :: Float,
		time :: Time
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
		duration :: Time
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
workoutTypeToString = (unlines . workoutTypeToTokens)

--tokens joined together on one line, split by |
workoutTypeToLine :: WorkoutType -> String
workoutTypeToLine = intercalate "|" . workoutTypeToTokens

workoutToString :: Workout -> String
workoutToString (Workout d w) = show d ++ " workout:\n" ++ indent (workoutTypeToString w)

workoutToLine :: Workout -> String
workoutToLine (Workout d w) = show d ++ " workout: " ++ workoutTypeToLine w

indent :: String -> String
indent = unlines . map ("\t"++) . lines
