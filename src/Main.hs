module Main where
import Ex1
import Ex2

-- Grupo:
-- André Luiz Kovalski
-- Gabrielle Louise
-- Leonardo Ikeda
-- Leonardo Knight

main :: IO ()
main = do  
  -- Para o funcionamento correto do 1, é imperativo que cada expressão esteja devidamente separada com espaços
  -- expressões validas para calculo:
  -- "sin" -  seno
  -- "cos" -  cosseno
  -- "tan" -  tangente
  -- "sqr" -  ao quadrado
  -- "cbc" -  ao cubo
  -- "sqrt" - raiz quadrada
  -- "cbct" - raiz cubica
  -- "+"  -   soma
  -- "-"  -   subtracao
  -- "*"  -   multiplicacao
  -- "/"  -   divisao
  -- "exp" -  x elevado a y
  print "Calculadora do Ex1" 
  print (interpret "( 3 6 - )")
  print (interpret "( 5 ( 3 1 - ) + )")
  print (interpret "( ( 5 3 + ) sqr )")
  print (interpret "( ( 5 3 + ) cbc )")
  print (interpret "( 3.4 ( 3.0 2 exp ) + )")
  print (interpret "( 2 6 exp ) ( 3 9 * ) /")
  print (interpret "( 69 4 exp ) ( 24 ( 3.14 -0.911 * ) + ) /")
  print (interpret "3.1415926 2 / sin")
  print (interpret "4 sqrt")
  print (interpret "27 cbct")
  print ""
  print "Validador do Ex2"
  print (validaCartao [1,2,3,4, 5,6,7,8, 9,0,1,2, 3,4,5,6])
  print (validaCartao [2,1,4,3, 6,5,8,7, 0,9,2,1, 8,3,0,5])
