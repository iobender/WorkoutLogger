import Workout
import ParseWorkouts

--for now we will just output the workout that was just entered 
--(in our format for the workout, of course)
main :: IO ()
main = getWorkout >>= (putStrLn . workoutToString)
