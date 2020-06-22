module Person
( Person(..)
) where


data Person = Jim { name :: String
	       , age :: Int
	       } deriving (Show)
