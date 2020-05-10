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

newtype Shape = Shape (Point -> Bool)   --needs only one constructor
type Point = (Double, Double) --type synonym

printFlashCard :: String -> IO ()
printFlashCard text = putStrLn $ tableString [fixedLeftCol 50, numCol] --[ColSpec]
                          asciiS --TableStyle
                          (titlesH ["Text"]) --HeaderSpec
                          [ colsAllG center [reverseParagraph $ justifyText 50 text]] --[RowGroup a]

word :: String -> String
word = head . words

meaning :: String -> String
meaning = unwords . tail . words

frame :: Shape
frame = Shape (\(x,y) -> x^512 + y^512 >=1)

inside :: Point -> Shape -> Bool
inside (x,y) (Shape f) = f (x,y)

scale :: Double -> Shape -> Shape
scale s (Shape f) = Shape  $ \(x,y) -> f (x/s, y/s)

render :: Shape -> Shape -> IO()
render outerFrame innerFrame = putStr $ unlines
--0.2500000000000011,0.20000000000000018
--0.8500000000000016,0.40000000000000013
--(0.10000000000000098,0.8),(0.15000000000000102,0.8),(0.20000000000000107,0.8),(0.2500000000000011,0.8)
            [ [ if x == midX && y == midY  then '✰'
                else if inside (x,y) outerFrame then '#'
                else if inside (x,y) innerFrame then '.'
                else ' ' | x <- [-1, -0.962 .. 1] ] | y <- [1, 0.9 .. -1] ]

reverseLine :: String -> String
reverseLine txt = unwords $ reverse $ words txt

reverseParagraph :: [String] -> [String]
reverseParagraph [] = []
reverseParagraph (x:xs) = reverseLine x : reverseParagraph xs

midX = [-1, -0.962 .. 1]!!(length [-1, -0.962 .. 1] `div` 2)
midY = [1, 0.9 .. -1]!!(length [1, 0.9 .. -1] `div` 2)

--Believe it or not, you are a diamond in the rough, and have always been a diamond in your own utterly unique and fantastic way. You might not have shone as brightly as you’ve wanted to at some points of your life, but that is probably because you were not in the right time and place in your life for that to happen. Scientifically, diamonds need a source of light that they can reflect, illuminating their near vicinity with a radiant light of their best qualities. In the lack of favorable motivation, resources, people, and situations, this light source may disappear, but hey, you don’t ever stop being a diamond! Sure, you might sometimes be a diamond in dormancy and inactivity- but you are constantly active in the process of rejuvenation and self-realization, thinking “Heck! Look at me! I am a diamond, even if the world gives me no credit and disregards me, telling me that I am a dull and lifeless rock.” Do not listen to the naysayers. You are indeed a rock, but you are a rock of shifting capabilities and an unrealized beauty. At the end of the day, it is up to you to interpret what kind of a rock you want to be. You are a diamond if you believe you are a diamond. You can and will shine brightly even when your opportunities look bleak. When you do shine bright, you might be in disbelief of your achievements. You might not be able to see yourself for how wonderful you are, but the world sees you in glory. The people that support you and appreciate you are the light in your life that you need to appreciate. Admittedly, at times, you need to be your own light to illuminate the inner works, the behind-the-scenes of the show that you put on for the world. Believing that you are a diamond is the first step. The rest will follow and fuel you forward with a self-belief that you are capable and will continue to be capable, regardless of what hardships that come your way.