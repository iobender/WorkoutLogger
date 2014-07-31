-- Matthew Bender
--
-- describes various types of workouts
module Workout where

import Data.List

type Time = Int

-- workout types
data Workout a where
	Workout :: WorkoutType w => w -> Workout w 
unWorkout :: WorkoutType w => Workout w -> w
unWorkout (Workout w) = w
instance Show (Workout w) where
	show (Workout w) = "Workout:\n" ++ (workoutTypeToString w)

data DistanceWorkout = 
	DistanceWorkout { 
		dwt :: DistanceWorkoutType,
		distance :: Float,
		time :: Time
	} deriving Show
data DistanceWorkoutType = Run | Bike | Swim | OtherDistance String deriving Show
instance WorkoutType DistanceWorkout where
	workoutTypeToTokens w = 
		["Type: " ++ (show $ dwt w)] ++ 
		["Distance: " ++ (show $ distance w)] ++ 
		["Time: " ++ (show $ time w)]

data CoreWorkout = 
	CoreWorkout {
		cwt :: CoreWorkoutType,
		coreReps :: Int
	} deriving Show
data CoreWorkoutType = Pushups | Crunches | SideDips | OtherCore String deriving Show
instance WorkoutType CoreWorkout where
	workoutTypeToTokens w = 
		["Type: " ++ (show $ cwt w)] ++
		["Reps: " ++ (show $ coreReps w)]

data WeightsWorkout = 
	WeightsWorkout {
		wwt :: WeightsWorkoutType,
		weight :: Int,
		weightReps :: Int
	} deriving Show
data WeightsWorkoutType = Curls | Bench | OtherWeights String deriving Show
instance WorkoutType WeightsWorkout where
	workoutTypeToTokens w = 
		["Type: " ++ (show $ wwt w)] ++ 
		["Weight: " ++ (show $ weight w)] ++ 
		["Reps: " ++ (show $ weightReps w)]

data SportsWorkout = 
	SportsWorkout {
		swt :: SportsWorkoutType,
		duration :: Time
	} deriving Show
data SportsWorkoutType = Baseball | Soccer | Frisbee | Football | OtherSports String deriving Show
instance WorkoutType SportsWorkout where
	workoutTypeToTokens w = 
		["Type: " ++ (show $ swt w)] ++
		["Duration: " ++ (show $ duration w)] 

class WorkoutType w where
	workoutTypeToTokens :: w -> [String]
	workoutTypeToString :: w -> String
	workoutTypeToString = (unlines . workoutTypeToTokens)
	workoutTypeToLine :: w -> String
	workoutTypeToLine wt = (intercalate s $ workoutTypeToTokens wt) ++ s
		where
		s = "|"

workoutToString :: WorkoutType w => Workout w -> String
workoutToString w = "Workout:\n" ++ (unlines $ map (\l -> "\t" ++ l) (lines $ workoutTypeToString $ unWorkout w))
workoutToLine :: WorkoutType w => Workout w -> String
workoutToLine w = "Workout: " ++ (workoutTypeToLine . unWorkout) w
