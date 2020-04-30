import Control.Monad

main = do
  line <- getLine
  --do stuff
  when (line /= "q" && line /= "Q") $ do
      main