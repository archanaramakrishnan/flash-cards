import Data.List
import System.Environment
import Control.Monad
--import Text.PrettyPrint.Boxes
import Text.Layout.Table

--main calls loop with an arg
--main = loop arg 
  --arg is the initial state

--printBox $ (/+/) (text "hello") (text "world")

{-main = do
  line <- getLine
  --do stuff
  Main.render (scale 0.8 frame) (scale 0.6 frame) "word" ((length "word")-1)
  when (line /= "q" && line /= "Q") $ do
      main-}

main = do
    let txt = "Believe it or not, you are a diamond in the rough, and have always been a diamond in your own utterly unique and fantastic way. You might not have shone as brightly as you’ve wanted to at some points of your life, but that is probably because you were not in the right time and place in your life for that to happen. Scientifically, diamonds need a source of light that they can reflect, illuminating their near vicinity with a radiant light of their best qualities. In the lack of favorable motivation, resources, people, and situations, this light source may disappear, but hey, you don’t ever stop being a diamond! Sure, you might sometimes be a diamond in dormancy and inactivity- but you are constantly active in the process of rejuvenation and self-realization, thinking “Heck! Look at me! I am a diamond, even if the world gives me no credit and disregards me, telling me that I am a dull and lifeless rock.” Do not listen to the naysayers. You are indeed a rock, but you are a rock of shifting capabilities and an unrealized beauty. At the end of the day, it is up to you to interpret what kind of a rock you want to be. You are a diamond if you believe you are a diamond. You can and will shine brightly even when your opportunities look bleak. When you do shine bright, you might be in disbelief of your achievements. You might not be able to see yourself for how wonderful you are, but the world sees you in glory. The people that support you and appreciate you are the light in your life that you need to appreciate. Admittedly, at times, you need to be your own light to illuminate the inner works, the behind-the-scenes of the show that you put on for the world. Believing that you are a diamond is the first step. The rest will follow and fuel you forward with a self-belief that you are capable and will continue to be capable, regardless of what hardships that come your way." in 
      putStrLn $ tableString [fixedLeftCol 50, numCol]
                          asciiS
                          (titlesH ["Text", "Length"])
                          [ colsAllG center [reverseParagraph $ justifyText 50 txt]]


newtype Shape = Shape (Point -> Bool)   --needs only one constructor
type Point = (Double, Double) --type synonym

frame :: Shape
frame = Shape (\(x,y) -> x^512 + y^512 >=1)

inside :: Point -> Shape -> Bool
inside (x,y) (Shape f) = f (x,y)

render :: Shape -> Shape -> String -> Int -> IO()
render outerFrame innerFrame text index = putStr $ unlines
            [ [ if (x == -0.95 &&  y == 1) then 'F'
                else if inside (x,y) outerFrame then '#'
                else if inside (x,y) innerFrame then '.'
                else ' ' | x <- [-1, -0.95 .. 1] ] | y <- [1, 0.9 .. -1] ]

scale :: Double -> Shape -> Shape
scale s (Shape f) = Shape  $ \(x,y) -> f (x/s, y/s)

reverseLine :: String -> String
reverseLine txt = unwords $ reverse $ words txt

reverseParagraph :: [String] -> [String]
reverseParagraph [] = []
reverseParagraph (x:xs) = reverseLine x : reverseParagraph xs