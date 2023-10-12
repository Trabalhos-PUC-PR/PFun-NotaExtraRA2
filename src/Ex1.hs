module Ex1 where
import Text.Read
import Data.Maybe (isJust)
-- Exercício 1:  CALCULADORA

-- Você deverá implementar, em Haskell, uma calculadora capaz de realizar, em números reais, as quatro operações, as operações trigonométricas (seno, cosseno e tangente) e as operações de exponenciação (quadrado, cubo, raiz quadrada, raiz cúbica e x elevado a y). Usando notação rpn de tal forma que todas as operações sejam representadas por uma S-expression. Como mostrado no exemplo
-- a seguir:

-- 𝑎) ( 3.4 5.0 +)
-- 𝑏) ( 3.4 (3.0 2 𝐸𝑋𝑃) +)

-- Caberá a você definir como irá separar os operandos de cada operação. Também caberá a você determinar as palavras chaves que usará para representar as operações trigonométricas (seno, cosseno e tangente) e as operações de exponenciação (quadrado, cubo, raiz quadrada, raiz cúbica e x elevado a y).

-- É importante observar que as S-expressions podem ser aninhadas, como pode ser visto no exemplo b e que não há limites para o aninhamento de expressões. Além disso, é importante lembrar que algumas operações solicitadas são unárias e outras são binárias.

data ExpressArg = Expression String | Value Float deriving (Show)

-- Getters e "Checkers"
isExpress :: ExpressArg -> Bool
isExpress (Expression s) = True
isExpress (Value v) = False 
isValue :: ExpressArg -> Bool
isValue (Expression s) = False
isValue (Value v) = True 

getExp :: ExpressArg -> String
getExp (Expression s) = s
getExp (Value v) = ""
getVal :: ExpressArg -> Float
getVal (Expression s) = -0
getVal (Value v) = v 

-- Verifica se a String é valida para ser um numero ou nao
isNumeric :: String -> Bool
isNumeric str = isJust(readMaybe str :: Maybe Float)

-- Pega o penultimo elemento de uma lista
last2 :: [a] -> a
last2 s = head (tail (reverse s))

-- Interpreta uma dada expressão e retorna seu resultado
interpret :: String -> Float
interpret s = calculator(parseString (words s)) []

-- Em palavras simples, converte valores numericos em um tipo e strings em outro, e devolve tudo como uma lista
parseString :: [String] -> [ExpressArg]
parseString s
  | length s > 1 && isNumeric (head s) = Value (read (head s)) : parseString (tail s)
  | length s > 1 && not(isNumeric (head s)) = Expression (head s) : parseString (tail s)
  | length s == 1 && isNumeric (head s) = [Value (read(head s))]
  | otherwise = [Expression (head s)]

-- Calcula dada expressão representada pelo tipo criado e devolve um float
calculator :: [ExpressArg] -> [Float] -> Float
calculator lista acc
  | null lista = sum acc
  | getExp(head lista)=="(" || getExp(head lista)==")" = calculator(tail lista) acc
  | isValue (head lista) = calculator(tail lista) ( acc++[getVal(head lista)] )
  | isExpress (head lista) = calculator(tail lista) ( applier (getExp(head lista)) acc)
  | otherwise = sum acc

-- Aplica funcoes de acordo com o valor da string passada
applier :: String -> [Float] -> [Float]
applier s v1
  | s == "sin" = init v1 ++ [sin (last v1)]                 -- seno
  | s == "cos" = init v1 ++ [cos (last v1)]                 -- cosseno
  | s == "tan" = init v1 ++ [tan (last v1)]                 -- tangente
  | s == "sqr" = init v1 ++ [last v1 ** 2]                  -- ao quadrado
  | s == "cbc" = init v1 ++ [last v1 ** 3]                  -- ao cubo
  | s == "sqrt" = init v1 ++ [last v1**(1/2)]                 -- raiz quadrada
  | s == "cbct" = init v1 ++ [last v1**(1/3)]                 -- raiz cubica
  | s == "+" = init (init v1) ++ [(+) (last2 v1) (last v1)] -- soma
  | s == "-" = init (init v1) ++ [(-) (last2 v1) (last v1)] -- subtracao
  | s == "*" = init (init v1) ++ [(*) (last2 v1) (last v1)] -- multiplicacao
  | s == "/" = init (init v1) ++ [(/) (last2 v1) (last v1)] -- divisao
  | s == "exp" = init (init v1) ++ [last2 v1 ** last v1]    -- elevado ao (potencia)
  | otherwise = [0] --invalid expression, always returns 

-- main :: IO ()
-- main = do
--   print (interpret "( 3 6 - )")
--   print (interpret "( 5 ( 3 1 - ) + )")
--   print (interpret "( ( 5 3 + ) cbc )")
--   print (interpret "( 3.4 ( 3.0 2 exp ) + )")
--   print (interpret "( 2 6 exp ) ( 3 9 * ) /")
--   print (interpret "( 69 2 exp ) ( 24 ( 3.14 -0.911 * ) + ) /")
--   print (interpret "( 3.1415926 2 / ) sin")
--   print (interpret " 4 sqrt")
--   print (interpret " 27 cbct")