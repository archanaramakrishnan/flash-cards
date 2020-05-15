import Data.List
import System.Environment
import Control.Monad
import Text.Layout.Table

main :: IO()
main = getFileName

{- to get file names from the terminal to read the flashcard question and answers from -}

getFileName :: IO()
getFileName = do
    args <- getArgs
    if length args == 0 then putStr "try ./flash_cards quiz.txt (from terminal) or :main quiz.txt (from ghci)\n" else do
        printWelcomePage args


{- to print the welcome page which calls the function to create the flashcards deck-}
  
printWelcomePage :: [FilePath] -> IO()
printWelcomePage args = do
    putStr "\n\n\tWelcome to the flash cards program!"
    putStr "\n\t___________________________________\n\n"
    putStr "\t  If you know the answer, press 'y'\n"
    putStr "\t   to keep track of your score!\n\n"
    putStr "\tIf you do not, press any other key ☆\n"
    putStr "\tCards will repeat until you learn them!\n\n"
    render (scale 0.8 frame) (scale 0.6 frame)
    putStr "\n\t  Press any key to start reviewing!\n"
    go <- getChar
    createQuestionList [] [] args args


{- to create a deck of flashcards, each line in the files representing one flashcard -}

createQuestionList :: [String] -> [String] -> [FilePath] -> [FilePath] -> IO ()
createQuestionList questionList answerList [] fileList = startFlashCards 0 questionList answerList fileList
createQuestionList (questionList) (answerList) (firstArg:argsList) fileList = do 
                    txt <- (readFile firstArg)
                    createQuestionList (questionList ++ (questionFromFile txt)) (answerList ++ (answerFromFile txt)) argsList fileList


{- to loop through the flash cards until the deck is empty -}

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


{- to read question and answers from the following format: 
    "question is placed here : answer goes here" from file -}

questionFromFile :: String -> [String]
questionFromFile txt = fmap getQuestion $ lines txt

getQuestion :: String -> String
getQuestion txt = beforeColon $ reverse $ words txt

beforeColon :: [String] -> String
beforeColon (x:y:xs) = if y == ":" then unwords $ reverse xs else beforeColon (y:xs)

answerFromFile :: String -> [String]
answerFromFile txt = fmap getAnswer $ lines txt

getAnswer :: String -> String
getAnswer txt = afterColon $ words txt

afterColon :: [String] -> String
afterColon (x:y:xs) = if y == ":" then unwords xs else afterColon (y:xs)


{- to print a single flashcard -}

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


{- to print a exit page with the score -}

printExitPage :: Int -> [FilePath] -> IO()
printExitPage score fileList = do
    putStr "You knew the answers to " >> (putStr $ show $ score) >> (printQuestionCount fileList 0)
    putStr "Thank you for using the flash cards program! ☆\n\n"

printQuestionCount :: [String] -> Int -> IO()
printQuestionCount [] count = putStr " out of the "  >> (putStr $ show $ count) >> putStr " flashcards!\n" 
printQuestionCount (firstArg:argsList) count = do
    txt <- (readFile firstArg)
    (printQuestionCount argsList ((linesCount txt) + count))

linesCount :: String -> Int
linesCount text = length $ lines text 


{- to create a frame instance of a shape to render the flash card on the welcome page -}

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