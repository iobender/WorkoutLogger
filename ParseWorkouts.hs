module ParseWorkouts where

import Workout

getWorkout :: WorkoutType w => IO (Workout w)
getWorkout = getLine >>= return . Workout . (\s -> if null s then DistanceWorkout Swim 1 30 else SportsWorkout Frisbee 100)
