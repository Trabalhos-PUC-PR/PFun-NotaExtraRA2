module Ex2 where
-- Exercício 2 VALIDAR CARTÃO DE CRÉDITO
-- No mundo real, o processo de validação de um número de cartão de crédito, é um pouco mais complexo. Caberá a você criar uma função capaz de validar estes números considerando a seguinte regra:
-- 1. Dobre o valor de cada segundo dígito, começando da direita, Ou seja, o último digito fica inalterado, o penúltimo é dobrado, o antepenúltimo não e assim sucessivamente de tal forma que [1,3,8,6] será transformado em [2,3,16,6].
-- 2. Some os dígitos com os valores dobrados, ou não: [2,3,16,6] será transformado em 18.
-- 3. Encontre o resto da divisão desta soma por 10. Neste caso 8.
-- 4. Se o resultado for zero, então este número é válido.
pegaDoisEmDois lista
  | length lista < 2 = []
  | otherwise = head (tail (reverse lista)) : pegaDoisEmDois (init (init lista))

validaCartao :: [Int] -> Bool
validaCartao lista
  | length lista < 2 = False
  | mod (2 * sum (pegaDoisEmDois lista)) 10 == 0 = True
  | otherwise = False

main :: IO ()
main = do  
  print (validaCartao [1,2,3,4, 5,6,7,8, 9,0,1,2, 3,4,5,6])