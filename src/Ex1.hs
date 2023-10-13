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
doExpressEqual (Expression s) str = s == str
doExpressEqual (Value v) str = isNumeric str && read str == v

-- | Gerencia qual ação sera realizada no acumulator de acordo com o arg da expressão
expressHandler :: ExpressArg -> [Float] -> [Float]
expressHandler (Expression s) acc = applier s acc
expressHandler (Value v) acc = acc ++ [v]

-- | Verifica se a String é valida para ser um numero ou nao
isNumeric :: String -> Bool
isNumeric str = isJust(readMaybe str :: Maybe Float)

-- | Pega o penultimo elemento de uma lista
last2 :: [a] -> a
last2 s = head (tail (reverse s))

-- | Interpreta uma dada expressão e retorna seu resultado como Float
interpret :: String -> Float
interpret "" = 0
interpret s = calculator(parseString (words s)) []

-- | Recebe uma lista de strings, e da parse nela de jeito que os valores numericos 
-- são um tipo, e expressões são outro, e uma lista com todos os valores "parseados"
parseString :: [String] -> [ExpressArg]
parseString s
  | length s > 1 && isNumeric (head s) = Value (read (head s)) : parseString (tail s)
  | length s > 1 && not(isNumeric (head s)) = Expression (head s) : parseString (tail s)
  | length s == 1 && isNumeric (head s) = [Value (read(head s))]
  | otherwise = [Expression (head s)]

-- | Calcula dada expressão representada pelo tipo criado e devolve um float
calculator :: [ExpressArg] -> [Float] -> Float
calculator lista acc
  | null lista = last acc
  | doExpressEqual(head lista) "(" || doExpressEqual(head lista) ")" = calculator(tail lista) acc
  | otherwise = calculator(tail lista) (expressHandler (head lista) acc)

-- | Aplica funcoes de acordo com o valor da expressão recebida, 
-- levanta erro caso a expressão seja invalida
applier :: String -> [Float] -> [Float]
applier exp acc
  | null acc = error ("interpretError - No value given to apply on ["++exp++"]") -- qtde de valores passados invalido
  | exp == "sin" = init acc ++ [sin (last acc)]                  -- seno
  | exp == "cos" = init acc ++ [cos (last acc)]                  -- cosseno
  | exp == "tan" = init acc ++ [tan (last acc)]                  -- tangente
  | exp == "sqr" = init acc ++ [last acc ** 2]                   -- ao quadrado
  | exp == "cbc" = init acc ++ [last acc ** 3]                   -- ao cubo
  | exp == "sqrt" = init acc ++ [last acc**(1/2)]                -- raiz quadrada
  | exp == "cbct" = init acc ++ [last acc**(1/3)]                -- raiz cubica
  | length acc == 1 = error ("interpretError - Binary expression ["++exp++"] got only one value to operate") -- qtde de valores passados invalido
  | exp == "+" = init (init acc) ++ [(+) (last2 acc) (last acc)] -- soma
  | exp == "-" = init (init acc) ++ [(-) (last2 acc) (last acc)] -- subtracao
  | exp == "*" = init (init acc) ++ [(*) (last2 acc) (last acc)] -- multiplicacao
  | exp == "/" = init (init acc) ++ [(/) (last2 acc) (last acc)] -- divisao
  | exp == "exp" = init (init acc) ++ [last2 acc ** last acc]    -- elevado ao (potencia)
  | otherwise = error ("interpretError - Invalid Expression ["++exp++"]") -- expressão invalida
