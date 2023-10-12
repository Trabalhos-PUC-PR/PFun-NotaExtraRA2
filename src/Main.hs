module Main where
import Ex1
import Ex2

main :: IO ()
main = do  
  print (validaCartao [1,2,3,4, 5,6,7,8, 9,0,1,2, 3,4,5,6])
