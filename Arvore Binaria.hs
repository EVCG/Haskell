data ArvoreBinaria a = Empty | Node a (ArvoreBinaria a) (ArvoreBinaria a) deriving(Show)
criarArvoreBinaria :: [a] -> Int -> ArvoreBinaria a
criarArvoreBinaria _ 0 = Empty
criarArvoreBinaria [] _ = Empty
criarArvoreBinaria(x:xs) h = Node x (criarArvoreBinaria arvoreEsquerdaVal arvoreEsquerdaAltura) (criarArvoreBinaria arvoreDireitaVal arvoreDireitaAltura)
  where
    arvoreEsquerdaAltura = h - 1
    arvoreDireitaAltura = h - 1
    (arvoreEsquerdaVal, arvoreDireitaVal) = splitAt (2 ^(arvoreEsquerdaAltura - 1)) xs


main :: IO()
main = do
  let vetor = [1..7]
  putStrLn "O vetor inicial é"
  print vetor
  let arvore = criarArvoreBinaria vetor 4
  putStrLn "A Arvore é "
  print arvore