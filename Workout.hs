-- Matthew Bender
--
-- describes various types of workouts
module Workout where

import Data.List

type Time = Int

-- workout types
-- fields with type wrapped in Maybe are optional
data Workout = Distance DistanceWorkout | Core CoreWorkout | Weights WeightsWorkout | Sports SportsWorkout deriving Show

data DistanceWorkout = 
	DistanceWorkout { 
		dwt :: DistanceWorkoutType,
		distance :: Float,
		time :: Time
	} deriving Show
data DistanceWorkoutType = Run | Bike | Swim | OtherDistance String deriving Show
instance WorkoutTypeLogger DistanceWorkout where
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
instance WorkoutTypeLogger CoreWorkout where
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
instance WorkoutTypeLogger WeightsWorkout where
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
instance WorkoutTypeLogger SportsWorkout where
	workoutTypeToTokens w = 
		["Type: " ++ (show $ swt w)] ++
		["Duration: " ++ (show $ duration w)] 

class WorkoutTypeLogger w where
	workoutTypeToTokens :: w -> [String]
	workoutTypeToString :: w -> String
	workoutTypeToString = (unlines . workoutTypeToTokens)
	workoutTypeToLine :: w -> String
	workoutTypeToLine wt = (intercalate s $ workoutTypeToTokens wt) ++ s
		where
		s = "|"

--workoutToString :: Workout -> String
--workoutToLine :: Workout -> String

-- test vars
run = Distance $ DistanceWorkout Run 3.1 17
Distance d = run
