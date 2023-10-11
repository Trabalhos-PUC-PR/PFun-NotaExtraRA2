module Main where
-- Exercício 2 VALIDAR CARTÃO DE CRÉDITO
-- No mundo real, o processo de validação de um número de cartão de crédito, é um pouco mais complexo. Caberá a você criar uma função capaz de validar estes números considerando a seguinte regra:
-- 1. Dobre o valor de cada segundo dígito, começando da direita, Ou seja, o último digito fica inalterado, o penúltimo é dobrado, o antepenúltimo não e assim sucessivamente de tal forma que [1,3,8,6] será transformado em [2,3,16,6].
-- 2. Some os dígitos com os valores dobrados, ou não: [2,3,16,6] será transformado em 18.
-- 3. Encontre o resto da divisão desta soma por 10. Neste caso 8.
-- 4. Se o resultado for zero, então este número é válido. 
pegaDoisEmDois lista
  | length lista < 2 = []
  | otherwise = head(tail(reverse lista)) : pegaDoisEmDois(init(init lista))

validaCartao :: [Int] -> Bool
validaCartao lista
  | length lista < 2 = False
  | mod (2 * sum (pegaDoisEmDois lista)) 10 == 0 = True
  | otherwise = False

-- Exercício 1:  CALCULADORA

-- Você deverá implementar, em Haskell, uma calculadora capaz de realizar, em números reais, as quatro operações, as operações trigonométricas (seno, cosseno e tangente) e as operações de exponenciação (quadrado, cubo, raiz quadrada, raiz cúbica e x elevado a y). Usando notação rpn de tal forma que todas as operações sejam representadas por uma S-expression. Como mostrado no exemplo
-- a seguir:

-- 𝑎) ( 3.4 5.0 +)
-- 𝑏) ( 3.4 (3.0 2 𝐸𝑋𝑃) +)

-- Caberá a você definir como irá separar os operandos de cada operação. Também caberá a você determinar as palavras chaves que usará para representar as operações trigonométricas (seno, cosseno e tangente) e as operações de exponenciação (quadrado, cubo, raiz quadrada, raiz cúbica e x elevado a y).

-- É importante observar que as S-expressions podem ser aninhadas, como pode ser visto no exemplo b e que não há limites para o aninhamento de expressões. Além disso, é importante lembrar que algumas operações solicitadas são unárias e outras são binárias.
-- calculaOp op
-- calculaOp = efetuaOp (words op)

-- efetuaOp 


  -- let cartao = [1,2,3,4 ,5,6,7,8, 9,1,2,3 ,4,5,6,7]
  -- print (validaCartao cartao)

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

parseSmallOp :: a -> (a -> a) -> a
parseSmallOp x f = f x
parseOp :: a -> a -> (a -> a -> a) -> a
parseOp x y f = f x y


applyF :: (a) -> (a) -> a  
applyF x f = f x


main :: IO ()
main = do  
  print (applyF (sqr 4) sqr)
  -- print (parseOp 1.5 3.5 (-))
  -- print (parseOp 1.5 3.5 (*))
  -- print (parseOp 1.5 3.5 (/))

  -- print (parseOp 1.5 3.5 exn)
  
  -- print (parseSmallOp 1.5 sin)
  -- print (parseSmallOp 1.5 cos)
  -- print (parseSmallOp 1.5 tan)

  
  -- print (parseSmallOp 1.5 sqr)
  -- print (parseSmallOp 1.5 cbc)
  -- print (parseSmallOp 1.5 sqR)
  -- print (parseSmallOp 1.5 cbR)
