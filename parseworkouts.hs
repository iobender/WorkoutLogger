import Workout

-- test vars
run = Workout $ DistanceWorkout Run 3.1 17
runW = unWorkout run

curl = Workout $ WeightsWorkout Curls 25 15
curlW = unWorkout curl

main :: IO ()
main = do
	(putStrLn . show) run
	(putStrLn . show) curl
