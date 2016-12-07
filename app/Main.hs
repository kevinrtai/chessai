module Main where
import Chess.Base
import Chess.Actions

main :: IO ()
main = do
  let a = Piece Queen Black
  let b = Piece Queen Black
  let c = Piece Rook Black
  let d = Piece Queen White
  putStrLn $ show (a == b)
  putStrLn $ show (a == c)
  putStrLn $ show (a == d)
