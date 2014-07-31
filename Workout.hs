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
		dwt :: DistanceWorkoutType,
		distance :: Float,
		time :: Time
	} | 
	Core {
		cwt :: CoreWorkoutType,
		coreReps :: Int
	} |
	Weights {
		wwt :: WeightsWorkoutType,
		weight :: Int,
		weightReps :: Int
	} |
	Sports {
		swt :: SportsWorkoutType,
		duration :: Time
	} 
	deriving (Eq, Show)

data DistanceWorkoutType = Run | Bike | Swim | OtherDistance String deriving (Eq, Show)
data CoreWorkoutType = Pushups | Crunches | SideDips | OtherCore String deriving (Eq, Show)
data WeightsWorkoutType = Curls | Bench | OtherWeights String deriving (Eq, Show)
data SportsWorkoutType = Baseball | Soccer | Frisbee | Football | OtherSports String deriving (Eq, Show)

workoutTypeToTokens :: WorkoutType -> [String]
workoutTypeToTokens w = case w of
	Distance _ _ _ -> 
		["Type: " ++ (show $ dwt w)] ++ 
		["Distance: " ++ (show $ distance w)] ++ 
		["Time: " ++ (show $ time w)]
	Core _ _ -> 
		["Type: " ++ (show $ cwt w)] ++
		["Reps: " ++ (show $ coreReps w)]
	Weights _ _ _ -> 
		["Type: " ++ (show $ wwt w)] ++ 
		["Weight: " ++ (show $ weight w)] ++ 
		["Reps: " ++ (show $ weightReps w)]
	Sports _ _ ->
		["Type: " ++ (show $ swt w)] ++
		["Duration: " ++ (show $ duration w)] 

workoutTypeToString :: WorkoutType -> String
workoutTypeToString = (unlines . workoutTypeToTokens)

workoutTypeToLine :: WorkoutType -> String
workoutTypeToLine = intercalate "|" . workoutTypeToTokens

workoutToString :: Workout -> String
workoutToString (Workout d w) = show d ++ " workout:\n" ++ indent (workoutTypeToString w)

workoutToLine :: Workout -> String
workoutToLine (Workout d w) = show d ++ " workout: " ++ workoutTypeToLine w

indent :: String -> String
indent = unlines . map ("\t"++) . lines
