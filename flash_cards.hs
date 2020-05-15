import Data.List
import System.Environment
import Control.Monad
import Text.Layout.Table

main :: IO()
main = printWelcomePage
  
loop :: IO()
loop = do
    args <- getArgs
    createQuestionList [] [] args

createQuestionList :: [String] -> [String] -> [FilePath] -> IO ()
createQuestionList questionList answerList [] = startFlashCards 0 questionList answerList
createQuestionList (questionList) (answerList) (firstArg:argsList) = do 
                    txt <- (readFile firstArg)
                    createQuestionList (questionList ++ (grabQuestion txt)) (answerList ++ (grabAnswer txt)) argsList

startFlashCards :: Int -> [String] -> [String] -> IO ()
startFlashCards score [] [] = putStr "You scored " >> (putStr $ show $ score) >> putStr " points!!\n" >> printExitPage score 0
startFlashCards score (q:questionList) (a:answerList) = do
            printFlashCard "Question" q
            putStr "\t  Press any key to reveal the answer. "
            reveal <- getChar
            if reveal /= '\n' then putStr "\n" else putStr ""
            printFlashCard q a
            putStr "\n\tPress 'y' - if you knew the answer!"
            putStr "\n\tPress 'q' - if you want to quit"
            putStr "\n\tPress any other key - to continue "
            choice <- getChar
            if choice /= '\n' then putStr "\n" else putStr ""
            putStr "\n\n\t\t    ☆ ☆ ☆ ☆ ☆\t\n\n"
            putStr "\n"
            if choice == 'q' || choice == 'Q' then printExitPage score 0
            else if choice == 'y' || choice == 'Y' then (startFlashCards (score+1) (questionList) (answerList))
            else (startFlashCards score (questionList ++ [q]) (answerList ++ [a]))

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

printExitPage :: Int -> Int -> IO()
printExitPage score total = do
    putStr "You knew the answers to " >> (putStr $ show $ score) >> putStr " out of the "  >> (putStr $ show $ total) >> putStr " flashcards!!\n" 
    putStr "Thank you for using the flash cards program! ☆\n\n"

printWelcomePage :: IO()
printWelcomePage = do
    putStr "\n\n\tWelcome to the flash cards program!"
    putStr "\n\t___________________________________\n\n"
    putStr "\t  If you know the answer, press 'y'\n"
    putStr "\t   to keep track of your score!\n\n"
    putStr "\tIf you do not, press any other key ☆\n"
    putStr "\tCards will repeat until you learn them!\n\n"
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