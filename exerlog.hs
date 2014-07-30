-- Matthew Bender
--
-- exerlog.hs: workout logger
import Data.Time.Clock

data Workout = 
	Run { 
		distance :: Float,
		time :: Int -- implement as an int # of seconds for now
	} | 
	Weights {
		weight :: Int,
		reps :: Int
	}
	deriving (Show)

