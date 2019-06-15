module Crypt_decrypt where

alf = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","A","B","C","D","E","F","sG","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","1","2","3","4","5","6","7","8","9","0"," ","\n"," ",""]

--CRYPT
--trocar o char de acordo com os alfabetos dado como entrada
encrypt_char :: [String] -> [String] -> String -> String
encrypt_char _ _ [] = []
encrypt_char (x:xs) (k:ks) char | x == char = k 
                                |otherwise  = encrypt_char xs ks char
--quebra uma string de acordo como um  determinado caractere                                 

split :: Char -> String -> [String]
split _ [] = [[]]
split c (x:xs) =  [x:takeWhile (/= c) (xs)] ++ split c (dropWhile (/= c) (xs)) 


--recebe uma string no alfabeto convencional e retorna ela em um alfabeto determinado
encrypt :: [String] -> String -> String
encrypt _ [] = []
encrypt cryp (x:xs) = encrypt_char alf cryp [x] ++ encrypt cryp xs


--DECRYPT

--recebe uma string em qual quer alfabeto (menos o natural) e retorna em qual quer alfabeto 
pre_decrypt :: [String] -> [String] -> [String] -> String 
pre_decrypt _ _ [] = []
pre_decrypt cod decod (x:xs) = encrypt_char cod decod x ++ pre_decrypt cod decod (xs)

--apena trata a entrada da função anterior
decrypt :: [String] -> [String] -> String -> String 
decrypt cod decod lista | head(lista) == ' ' = pre_decrypt cod decod (split ' ' (lista))
                        | otherwise          = pre_decrypt cod decod (split ' ' (" " ++ lista))


cifra_de_cesar :: Int -> [String]
cifra_de_cesar chave = drop chave alf ++ take chave alf
