module ParseWorkouts where

import Workout

getWorkout :: IO Workout
--placeholder for actual parsing of input
getWorkout = return $ Workout Easy $ Sports Frisbee 100
