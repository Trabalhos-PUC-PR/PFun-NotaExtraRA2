module Ex1 where
import Data.String
-- Exercício 1:  CALCULADORA

-- Você deverá implementar, em Haskell, uma calculadora capaz de realizar, em números reais, as quatro operações, as operações trigonométricas (seno, cosseno e tangente) e as operações de exponenciação (quadrado, cubo, raiz quadrada, raiz cúbica e x elevado a y). Usando notação rpn de tal forma que todas as operações sejam representadas por uma S-expression. Como mostrado no exemplo
-- a seguir:

-- 𝑎) ( 3.4 5.0 +)
-- 𝑏) ( 3.4 (3.0 2 𝐸𝑋𝑃) +)

-- Caberá a você definir como irá separar os operandos de cada operação. Também caberá a você determinar as palavras chaves que usará para representar as operações trigonométricas (seno, cosseno e tangente) e as operações de exponenciação (quadrado, cubo, raiz quadrada, raiz cúbica e x elevado a y).

-- É importante observar que as S-expressions podem ser aninhadas, como pode ser visto no exemplo b e que não há limites para o aninhamento de expressões. Além disso, é importante lembrar que algumas operações solicitadas são unárias e outras são binárias.

data ExpressArg = Expression String | Value Float deriving (Show)

smm :: Float -> Float -> Float
smm = (+)
sub :: Float -> Float -> Float
sub = (-)
tim :: Float -> Float -> Float
tim = (*)
dis :: Float -> Float -> Float
dis = (/)
exn :: (Floating a) => a -> a -> a
exn v1 v2 = v1 ** v2
sqr :: (Floating a) => a -> a 
sqr = (**2)
cbc :: (Floating a) => a -> a 
cbc = (**3)
sqR :: (Floating a) => a -> a 
sqR = (**(1/2))
cbR :: (Floating a) => a -> a 
cbR = (**(1/3))

isOpenParenth :: ExpressArg -> Bool
isOpenParenth (Expression s)
  | s == "(" = True
  | otherwise = False
isOpenParenth (Value v) = False 
isClosedParenth :: ExpressArg -> Bool
isClosedParenth (Expression s)
  | s == ")" = True
  | otherwise = False
isClosedParenth (Value v) = False 

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
getVal (Expression s) = 0.0
getVal (Value v) = v 
  
isNumeric :: String -> Bool
isNumeric str = case reads str :: [(Double, String)] of
               [(_, "")] -> True
               _ -> False
isNotNumeric :: String -> Bool
isNotNumeric str = case reads str :: [(Double, String)] of
               [(_, "")] -> False
               _ -> True

first :: [a] -> a
first = head
last2 :: [a] -> a
last2 s = head (tail (reverse s))

interpret :: String -> Float
interpret s = calculator(parseString (words s)) []

parseString :: [String] -> [ExpressArg]
parseString s
  | length s > 1 && isNumeric (first s) = Value (read (head s)) : parseString (tail s)
  | length s > 1 && isNotNumeric (first s) = Expression (head s) : parseString (tail s)
  | length s == 1 && isNumeric (first s) = [Value (read(head s))]
  | otherwise = [Expression (head s)]

calculator :: [ExpressArg] -> [Float] -> Float
calculator lista acc
  | null lista = sum acc
  | isOpenParenth (first lista) || isClosedParenth (first lista) = calculator(tail lista) acc
  | isValue (first lista) = calculator(tail lista) ( acc++[getVal(first lista)] )
  | isExpress (first lista) = calculator(tail lista) ( applier (getExp(first lista)) acc)
  | otherwise = sum acc

applier :: String -> [Float] -> [Float]
applier s v1
  | s == "sin" = init v1 ++ [sin (last v1)]
  | s == "cos" = init v1 ++ [cos (last v1)]
  | s == "tan" = init v1 ++ [tan (last v1)]
  | s == "sqr" = init v1 ++ [sqr (last v1)]
  | s == "cbc" = init v1 ++ [cbc (last v1)]
  | s == "sqR" = init v1 ++ [sqR (last v1)]
  | s == "cbR" = init v1 ++ [cbR (last v1)]
  | s == "smm" = init (init v1) ++ [smm (last2 v1) (last v1)]
  | s == "sub" = init (init v1) ++ [sub (last2 v1) (last v1)]
  | s == "tim" = init (init v1) ++ [tim (last2 v1) (last v1)]
  | s == "dis" = init (init v1) ++ [dis (last2 v1) (last v1)]
  | s == "exn" = init (init v1) ++ [exn (last2 v1) (last v1)]
  | otherwise = [123.321] --invalid expression, always returns this strange number

main :: IO ()
main = do
  let exp1 = "3 6 sub"
  let exp2 = "( 5 ( 3 1 sub ) smm )"
  let exp3 = "( ( 5 3 smm ) cbc )"
  let exp4 = "( 3.4 ( 3.0 2 exn ) smm )"
  let exp5 = "( 2 6 exn ) ( 3 9 tim ) dis"
  let exp6 = "( 69 2 exn ) ( 24 ( 3.14 -0.911 tim ) smm ) dis"
  print (interpret exp1)
  print (interpret exp2)
  print (interpret exp3)
  print (interpret exp4)
  print (interpret exp5)
  print (interpret exp6)