import Workout
import ParseWorkouts

-- test vars
runW = Distance Run 3.1 17
run = Workout Hard runW

curlW = Weights Curls 25 15
curl = Workout Easy curlW 

--for now we will just output the workout that was just entered 
--(in our format for the workout, of course)
main :: IO ()
main = getWorkout >>= (putStrLn . workoutToString)
