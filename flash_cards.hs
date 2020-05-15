import Data.List
import System.Environment
import Control.Monad
import Text.Layout.Table

main :: IO()
main = printWelcomePage
  
getFileName :: IO()
getFileName = do
    args <- getArgs
    createQuestionList [] [] args args

getQuestionCount :: [String] -> Int -> IO()
getQuestionCount [] count = putStr " out of the "  >> (putStr $ show $ count) >> putStr " flashcards!!\n" 
getQuestionCount (firstArg:argsList) count = do
    txt <- (readFile firstArg)
    (getQuestionCount argsList ((linesCount txt) + count))

linesCount :: String -> Int
linesCount text = length $ lines text 

createQuestionList :: [String] -> [String] -> [FilePath] -> [FilePath] -> IO ()
createQuestionList questionList answerList [] fileList = startFlashCards 0 questionList answerList fileList
createQuestionList (questionList) (answerList) (firstArg:argsList) fileList = do 
                    txt <- (readFile firstArg)
                    createQuestionList (questionList ++ (grabQuestion txt)) (answerList ++ (grabAnswer txt)) argsList fileList

startFlashCards :: Int -> [String] -> [String] -> [FilePath] -> IO ()
startFlashCards score [] [] fileList = printExitPage score fileList
startFlashCards score (q:questionList) (a:answerList) fileList = do
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
            if choice == 'q' || choice == 'Q' then printExitPage score fileList
            else if choice == 'y' || choice == 'Y' then (startFlashCards (score+1) (questionList) (answerList) fileList)
            else (startFlashCards score (questionList ++ [q]) (answerList ++ [a]) fileList)

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

printExitPage :: Int -> [FilePath] -> IO()
printExitPage score fileList = do
    putStr "You knew the answers to " >> (putStr $ show $ score) >> (getQuestionCount fileList 0)
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
    getFileName

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