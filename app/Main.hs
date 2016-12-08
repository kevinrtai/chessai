module Main where
import Chess.Base
import Chess.Actions

main :: IO ()
main = do
  let a = Piece Queen Black 0
  let b = Piece Queen Black 0
  let c = Piece Rook Black 0
  let d = Piece Queen White 0
  putStrLn $ show (a == b)
  putStrLn $ show (a == c)
  putStrLn $ show (a == d)
