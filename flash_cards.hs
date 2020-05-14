import Data.List
import System.Environment
import Control.Monad
import Text.Layout.Table

main :: IO()
main = printWelcomePage
  
loop :: IO()
loop = do
    args <- getArgs
    readFromFile args

readFromFile :: [FilePath] -> IO ()
readFromFile [] = printExitPage
readFromFile (firstArg:argsList) = do 
                    txt <- (readFile firstArg)
                    let questionList = grabQuestion txt
                    let answerList = grabAnswer txt
                    startFlashCards questionList answerList
                    readFromFile (argsList)

startFlashCards :: [String] -> [String] -> IO ()
startFlashCards [] [] = putStr "\n"
startFlashCards (q:questionList) (a:answerList) = do
                printFlashCard "What is" (q ++ "?")
                putStr "\t  Press any key to reveal the answer. "
                reveal <- getChar
                if reveal /= '\n' then putStr "\n" else putStr ""
                printFlashCard q a
                putStr "\tEnter any key to go to the next flashcard."
                putStr "\n\tEnter 'q' to quit. "
                choice <- getChar
                putStr "\n\n\n\t\t    ☆ ☆ ☆ ☆ ☆\t\n\n"
                putStr "\n"
                if choice == 'q' || choice == 'Q' then putStr ""
                else (startFlashCards (questionList) (answerList))

grabQuestion :: String -> [String]
grabQuestion txt = fmap word lst where lst = lines txt

word :: String -> String
word = head . words

grabAnswer :: String -> [String]
grabAnswer txt = fmap meaning lst where lst = lines txt

meaning :: String -> String
meaning = unwords . tail . words

executeChoice :: Char -> IO()
executeChoice txt = putChar txt

printFlashCard :: String -> String -> IO ()
printFlashCard question answer = putStrLn $ tableString [fixedLeftCol 50, numCol] --[ColSpec]
                          asciiS --TableStyle
                          (titlesH [question]) --HeaderSpec
                          [ colsAllG center [reverseParagraph $ justifyText 50 answer]] --[RowGroup a]

reverseLine :: String -> String
reverseLine txt = unwords $ reverse $ words txt

reverseParagraph :: [String] -> [String]
reverseParagraph [] = []
reverseParagraph (x:xs) = reverseLine x : reverseParagraph xs

printExitPage :: IO()
printExitPage = putStr "Thank you for using the flash cards program! ☆\n\n"

printWelcomePage :: IO()
printWelcomePage = do
    putStr "\n\n\tWelcome to the flash cards program!"
    putStr "\n\t___________________________________\n"
    putStr "\n\t    Enter 'r' to reveal the answer. \n"
    putStr "\tIf you know the answer, enter 'y'\n"
    putStr "\tIf you do not know the answer enter 'n'.\n"
    render (scale 0.8 frame) (scale 0.6 frame)
    putStr "\n\t  Press any key to start reviewing!\n"
    go <- getChar
    loop

newtype Shape = Shape (Point -> Bool)
type Point = (Double, Double)

frame :: Shape
frame = Shape (\(x,y) -> x^512 + y^512 >=1)

render :: Shape -> Shape -> IO()
render outerFrame innerFrame = putStr $ unlines
            [ [      if x == midX && y == midY  then '☆'
                else if inside (x,y) outerFrame then '#'
                else if inside (x,y) innerFrame then '.'
                else ' ' | x <- [-1, -0.962 .. 1] ] | y <- [1, 0.9 .. -1] ]

inside :: Point -> Shape -> Bool
inside (x,y) (Shape f) = f (x,y)

scale :: Double -> Shape -> Shape
scale s (Shape f) = Shape  $ \(x,y) -> f (x/s, y/s)

midX :: Double
midX = let midIndex = (length lst `div` 2) in lst!!midIndex  where lst = [-1, -0.962 .. 1]

midY :: Double
midY = let midIndex = (length lst `div` 2) in lst!!midIndex where lst = [1, 0.9 .. -1]