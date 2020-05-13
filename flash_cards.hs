import Data.List
import System.Environment
import Control.Monad
import Text.Layout.Table

--main calls loop with an arg

main :: IO()
main = printWelcomePage >> loop
  
loop :: IO()
loop = do
    args <- getArgs
    readFromFile args
    line <- getLine
    when (line /= "Quit" && line /= "quit" && line /= "QUIT") $ do
        --printFlashCard "Scientifically, diamonds need a source of light that they can reflect, illuminating their near vicinity with a radiant light of their best qualities. In the lack of favorable motivation, resources, people, and situations, this light source may disappear, but hey, you don’t ever stop being a diamond!"
        loop


readFromFile [] = putStr "Done in readFromFile!\n"
readFromFile (firstArg:argsList) = do 
                    txt <- (readFile firstArg)
                    let questionList = grabQuestion txt
                    let answerList = grabAnswer txt
                    startFlashCards questionList answerList
                    readFromFile (argsList)
                    --putStr txt
                    --readFromFile argsList

startFlashCards [] [] = putStr "\nDone in startFlashCards!\n"
startFlashCards (q:questionList) (a:answerList) = do
                                    printFlashCard q
                                    printFlashCard a
                                    line <- getLine
                                    when (line == "n") $ do
                                        startFlashCards questionList answerList
{-readFromFile args =  sequence_ [ do
            txt <- readFile fileName
            (printFlashCard ((grabQuestion txt)!!0)) | fileName <- args]-}

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

printFlashCard :: String -> IO ()
printFlashCard text = putStrLn $ tableString [fixedLeftCol 50, numCol] --[ColSpec]
                          asciiS --TableStyle
                          (titlesH ["Text"]) --HeaderSpec
                          [ colsAllG center [reverseParagraph $ justifyText 50 text]] --[RowGroup a]

reverseLine :: String -> String
reverseLine txt = unwords $ reverse $ words txt

reverseParagraph :: [String] -> [String]
reverseParagraph [] = []
reverseParagraph (x:xs) = reverseLine x : reverseParagraph xs

printWelcomePage :: IO()
printWelcomePage = do
    putStr "\n\n\tWelcome to the flash cards program!\n"
    render (scale 0.8 frame) (scale 0.6 frame)
    putStr "\n\n   Enter a lower case letter to review words: \n"
    --choice <- getChar
    --executeChoice choice

newtype Shape = Shape (Point -> Bool)
type Point = (Double, Double)

frame :: Shape
frame = Shape (\(x,y) -> x^512 + y^512 >=1)

render :: Shape -> Shape -> IO()
render outerFrame innerFrame = putStr $ unlines
            [ [      if x == midX && y == midY  then '✰'
                else if inside (x,y) outerFrame then '#'
                else if inside (x,y) innerFrame then '.'
                else ' ' | x <- [-1, -0.962 .. 1] ] | y <- [1, 0.9 .. -1] ]

inside :: Point -> Shape -> Bool
inside (x,y) (Shape f) = f (x,y)

scale :: Double -> Shape -> Shape
scale s (Shape f) = Shape  $ \(x,y) -> f (x/s, y/s)

midX :: Double
midX = let midIndex = (length lst `div` 2) in 
        lst!!midIndex 
        where lst = [-1, -0.962 .. 1]

midY :: Double
midY = let midIndex = (length lst `div` 2) in 
        lst!!midIndex 
        where lst = [1, 0.9 .. -1]