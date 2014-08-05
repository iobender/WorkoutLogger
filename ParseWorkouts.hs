module ParseWorkouts where

import Data.List
import Data.Char
import Text.Read
import System.IO
import System.Locale
import Data.Time
import Data.Time.Format
import Data.Time.Clock
import Data.Time.Calendar
import Workout

--formats whitespace to just single spaces between words
basestr :: String -> String
basestr = unwords . words

--counts the times a predicate is true in a list
count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

--splits a list where a predicate is true
splitP :: (a -> Bool) -> [a] -> [[a]]
splitP p s = case dropWhile p s of 
	[] -> []
	s' -> w : splitP p s''
		where (w, s'') = break p s'

--lookup ignoring case and whitespace
lookup' :: String -> [(String, a)] -> Maybe a
lookup' = lookup . basestr . map toLower

stdinLoop :: String -> String -> (String -> Maybe a) -> IO a
stdinLoop prompt infoMsg parseFunc =
	putStr prompt >> hFlush stdout >>
	getLine >>=
	(\s -> 
	 	if s == "..." then 
			putStrLn infoMsg >> stdinLoop prompt infoMsg parseFunc
		else
			case parseFunc s of 
				Nothing -> stdinLoop prompt infoMsg parseFunc
				Just x -> return x
	)

--reads a line from stdin that contains 1+ alpha-numeric character
getString :: String -> IO String
getString p =
	putStr p >> hFlush stdout >>
	getLine >>=
	(\s -> 
	 	if s == "..." then putStrLn "String" >> getString p else
		if s /= "-" && count isAlphaNum s == 0 then getString p
		else return (basestr s))

--reads from stdin until the line is readable for the correct type
getRead :: Read a => String -> String -> IO a
getRead p t = 
	putStr p >> hFlush stdout >> 
	getLine >>= 
	(\s -> 
	 	if s == "..." then putStrLn t >> getRead p t else
	 	case readMaybe (basestr s) of
	 	Nothing -> getRead p t
		Just v -> return v)

--prints the keys of an association list
putKeys :: [(String, a)] -> IO ()
putKeys m = putStrLn $ concat $ map (\(k,_) -> k ++ " ") m

--given an association table, reads stdin until the one of the keys is 
--entered, ignoring case and whitespace, and returns that value
getLookup :: String -> [(String, a)] -> IO a
getLookup p m = 
	putStr p >> hFlush stdout >> 
	getLine >>=
	(\s -> if s == "..." then putKeys m >> getLookup p m else
		case lookup' (basestr s) m of
	 		Nothing -> getLookup p m 
			Just v -> return v)

--reads in a date time matching a given format
getDateTime :: String -> String -> String -> IO UTCTime
getDateTime p f t = 
	putStr p >> hFlush stdout >>
	getLine >>= 
	(\s -> 	
	 	if s == "..." then putStrLn t >> getDateTime p f t else
	 	case parseTime defaultTimeLocale f (basestr s) of
	 		Nothing -> getDateTime p f t
			Just v -> return v)

--reads in a time as colon separated numbers, returning it as an int
getIntTime :: String -> String -> IO Int
getIntTime p f = 
	putStr p >> hFlush stdout >>
	getLine >>=
	(\s -> if s == "..." then putStrLn f >> getIntTime p f else
	 	case parseIntTime (basestr s) of 
			Nothing -> getIntTime p f
			Just n -> return n)

--parses a string of colon separated numbers as a list
--does limited error checking
parseIntTime :: String -> Maybe Int
parseIntTime s = 
	let times = splitP (== ':') s in
	if length times == 0 
		|| any (\ts -> length ts > 2 ||
			any (not . isDigit) ts) times
		then Nothing 
	else
		let ns = map read times :: [Int] in 
		if any (\n -> n >= 60) ns then Nothing else
		Just (foldl' (\a n -> 60 * a + n) 0 ns)

--gets an entire workout from stdin. Main function to be called
getWorkout :: IO Workout
getWorkout = do
	wt <- getWorkoutTypeFunc >>= (\f -> f >>= return)
	com <- getCommon
	return $ Workout wt com

--the below are helper functions to aid in getting a Workout

getWorkoutTypeFunc :: IO (IO WorkoutType)
getWorkoutTypeFunc = getLookup "type: " [("distance", getDistanceWorkout), ("core", getCoreWorkout), ("weights", getWeightsWorkout), ("sports", getSportsWorkout)]	       

getDistanceWorkout = do
	dw <- getLookup "dist type: " [("run", Run), ("bike", Bike), ("swim", Swim)]
	distance <- getRead "distance: " "Float"
	seconds <- getIntTime "time: " "Time: MM:SS or HH:MM:SS"
	return $ Distance dw distance seconds

getCoreWorkout = do
	cw <- getLookup "core type: " [("pushups", Pushups), ("crunches", Crunches), ("sidedips", SideDips)]
	coreReps <- getRead "reps: " "Integer"
	return $ Core cw coreReps

getWeightsWorkout = do
	ww <- getLookup "weights type: " [("curls", Curls), ("bench", Bench)]
	weight <- getRead "weight: " "Integer" 
	weightReps <- getRead "reps: " "Integer"
	return $ Weights ww weight weightReps

getSportsWorkout = do
	sw <- getLookup "sports type: " [("baseball", Baseball), ("soccer", Soccer), ("frisbee", Frisbee), ("football", Football)]
	minutes <- getIntTime "time: " "Time: MM or HH:MM"
	return $ Sports sw minutes

getCommon :: IO Common
getCommon = do
	date <- getDateTime "date: " "%-m/%-d" "Date: MM:DD"
	year <- getCurrentTime 
	time <- getDateTime "time of day: " "%-H:%M" "Time: HH:DD"
	place <- getString "place: "
	weather <- getString "weather: "
	diff <- getDifficulty
	gear <- getString "gear: "
	return $ Common (buildDateTime date year time) place weather diff gear 

buildDateTime :: UTCTime -> UTCTime -> UTCTime -> UTCTime
buildDateTime date year time = 
	readTime defaultTimeLocale "%-m/%-d/%y %H:%M" $ formatTime defaultTimeLocale "%m/%d/" date ++ formatTime defaultTimeLocale "%y " year ++ formatTime defaultTimeLocale "%H:%M" time

getDifficulty :: IO Difficulty
getDifficulty = getLookup "difficulty: " [("easy", Easy), ("medium", Medium), ("hard", Hard)]

