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

-- | tipo de dado que representa o argumento de uma expressão, pode ser ou uma Expressão, ou um Valor
data ExpressArg = Expression String | Value Float deriving (Show)

-- | Checa se dado argumento da expressão é igual a dada string
doExpressEqual :: ExpressArg -> String -> Bool
doExpressEqual (Expression express) str = express == str
doExpressEqual (Value val) str = isNumeric str && read str == val

-- | Gerencia qual ação sera realizada no acumulator de acordo com o arg da expressão
expressHandler :: ExpressArg -> [Float] -> [Float]
expressHandler (Expression express) acc = applier express acc
expressHandler (Value val) acc = acc ++ [val]

-- | Verifica se a String é valida para ser um numero ou nao
isNumeric :: String -> Bool
isNumeric str = isJust(readMaybe str :: Maybe Float)

-- | Pega o penultimo elemento de uma lista
last2 :: [a] -> a
last2 list = head (tail (reverse list))

-- | Interpreta uma dada expressão e retorna seu resultado como Float
interpret :: String -> Float
interpret "" = 0
interpret string = calculator(parseString (words string)) []

-- | Recebe uma lista de string, e devolve uma lista de argumentos da expressão, que são ou Values ou Expressions
parseString :: [String] -> [ExpressArg]
parseString [] = []
parseString list
  | isNumeric (head list) = Value (read (head list)) : parseString (tail list)
  | not(isNumeric (head list)) = Expression (head list) : parseString (tail list)

-- | Calcula dada expressão representada pelo tipo criado e devolve um float
calculator :: [ExpressArg] -> [Float] -> Float
calculator [] acc = last acc
calculator list acc
  | doExpressEqual(head list) "(" || doExpressEqual(head list) ")" = calculator(tail list) acc
  | otherwise = calculator(tail list) (expressHandler (head list) acc)

-- | Aplica funcoes de acordo com o valor da expressão recebida, levanta erro caso a expressão seja invalida
applier :: String -> [Float] -> [Float]
applier express acc
  | null acc = error ("interpretError - No value given to apply on ["++express++"]") -- qtde de valores passados invalido
  | express == "sin" = init acc ++ [sin (last acc)]                  -- seno
  | express == "cos" = init acc ++ [cos (last acc)]                  -- cosseno
  | express == "tan" = init acc ++ [tan (last acc)]                  -- tangente
  | express == "sqr" = init acc ++ [last acc ** 2]                   -- ao quadrado
  | express == "cbc" = init acc ++ [last acc ** 3]                   -- ao cubo
  | express == "sqrt" = init acc ++ [last acc**(1/2)]                -- raiz quadrada
  | express == "cbct" = init acc ++ [last acc**(1/3)]                -- raiz cubica
  | length acc == 1 = error ("interpretError - Binary expression ["++express++"] got only one value to operate") -- qtde de valores passados invalido
  | express == "+" = init (init acc) ++ [(+) (last2 acc) (last acc)] -- soma
  | express == "-" = init (init acc) ++ [(-) (last2 acc) (last acc)] -- subtracao
  | express == "*" = init (init acc) ++ [(*) (last2 acc) (last acc)] -- multiplicacao
  | express == "/" = init (init acc) ++ [(/) (last2 acc) (last acc)] -- divisao
  | express == "exp" = init (init acc) ++ [last2 acc ** last acc]    -- elevado ao (potencia)
  | otherwise = error ("interpretError - Invalid Expression ["++express++"]") -- expressão invalida
