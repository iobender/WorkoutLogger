-- Matthew Bender
--
-- describes various types of workouts
module Workout where

import Data.List

type Time = Int

data Data = D1 { n :: Int } | D2 { b :: Bool } | D3 { s :: String } deriving Show

-- workout types
-- fields with type wrapped in Maybe are optional
--data Workout = Distance DistanceWorkout | Core CoreWorkout | Weights WeightsWorkout | Sports SportsWorkout deriving Show
--data Workout w => WorkoutType w = Workout w
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

workoutToString :: Workout w -> String
workoutToString (Workout w) = "Workout:\n" ++ (unlines $ map (\l -> "\t" ++ l) (lines $ workoutTypeToString $ w))
workoutToLine :: Workout w -> String
workoutToLine (Workout w) = "Workout: " ++ workoutTypeToLine w

-- test vars
run = Workout $ DistanceWorkout Run 3.1 17
runW = unWorkout run

curl = Workout $ WeightsWorkout Curls 25 15
curlW = unWorkout curl
