module Utilities where
    import Text.Read
    import Data.Maybe
    import Data.Text.IO
    import Data.Text (unpack)

    -- Get list of lines from file
    getLines :: String -> IO [String]
    getLines s = do
        -- get the lines of the file
        contents <- Data.Text.IO.readFile s
        let linesOfFile = lines (unpack contents)
        -- return the lines
        return linesOfFile
    
    -- Function to convert a string to (maybe) an int
    toInt :: String -> Maybe Int
    toInt i = readMaybe i :: Maybe Int

    -- Function to convert a list of strings to a list of maybe ints
    toInts :: [String] -> [Maybe Int]
    toInts s = map toInt s

    -- Get list of ints from a file
    getInts :: String -> IO [Maybe Int]
    getInts s = do
        lines <- getLines s
        return (toInts lines)