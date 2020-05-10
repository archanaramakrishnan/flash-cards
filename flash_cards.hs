import Data.List
import System.Environment
import Control.Monad
import Text.Layout.Table

--main calls loop with an arg

main :: IO()
main = printWelcomePage >> loop
  
loop :: IO()
loop = do
    line <- getLine
    when (line /= "Quit" && line /= "quit" && line /= "QUIT") $ do
        printFlashCard "Scientifically, diamonds need a source of light that they can reflect, illuminating their near vicinity with a radiant light of their best qualities. In the lack of favorable motivation, resources, people, and situations, this light source may disappear, but hey, you don’t ever stop being a diamond!"
        loop

printWelcomePage :: IO()
printWelcomePage = do
    putStr "\n\n\tWelcome to the flash cards program!\n"
    render (scale 0.8 frame) (scale 0.6 frame)
    putStr "\n\n   Enter a lower case letter to review words: \n"
    choice <- getChar
    executeChoice choice

executeChoice :: Char -> IO()
executeChoice txt = putChar txt


printFlashCard :: String -> IO ()
printFlashCard text = putStrLn $ tableString [fixedLeftCol 50, numCol] --[ColSpec]
                          asciiS --TableStyle
                          (titlesH ["Text"]) --HeaderSpec
                          [ colsAllG center [reverseParagraph $ justifyText 50 text]] --[RowGroup a]



word :: String -> String
word = head . words

meaning :: String -> String
meaning = unwords . tail . words

reverseLine :: String -> String
reverseLine txt = unwords $ reverse $ words txt

reverseParagraph :: [String] -> [String]
reverseParagraph [] = []
reverseParagraph (x:xs) = reverseLine x : reverseParagraph xs

newtype Shape = Shape (Point -> Bool)
type Point = (Double, Double)

frame :: Shape
frame = Shape (\(x,y) -> x^512 + y^512 >=1)

render :: Shape -> Shape -> IO()
render outerFrame innerFrame = putStr $ unlines
            [ [ if x == midX && y == midY  then '✰'
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