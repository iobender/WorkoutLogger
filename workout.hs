-- Matthew Bender
--
-- describes various types of workouts

module Workout where

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

data CoreWorkout = 
	CoreWorkout {
		cwt :: CoreWorkoutType,
		coreReps :: Int
	} deriving Show
data CoreWorkoutType = Pushups | Crunches | SideDips | OtherCore String deriving Show

data WeightsWorkout = 
	WeightsWorkout {
		wwt :: WeightsWorkoutType,
		weight :: Int,
		weightReps :: Int
	} deriving Show
data WeightsWorkoutType = Curls | Bench | OtherWeights String deriving Show

data SportsWorkout = 
	SportsWorkout {
		swt :: SportsWorkoutType,
		duration :: Time
	} deriving Show
data SportsWorkoutType = Baseball | Soccer | Frisbee | Football | OtherSports String deriving Show

class WorkoutLogger w where
	workoutToString :: w -> String
	workoutToLine :: w -> String


-- test vars
run = Distance $ DistanceWorkout Run 3.1 17
