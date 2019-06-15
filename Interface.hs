import System.IO
import System.Process
import Data.Char 
import Data.List
import Data.List (transpose)
import Alphabet
import Crypt_decrypt

encripitar :: IO()
encripitar = do
        system("clear")
        putStrLn "Digite a sequência que será decodificada."
        putStrLn "OBS: Por favor digite caracteres pertencentes ao alfabeto (exemplo de não aceitos: ã, ~ , !,@ ,# ,.)"
        seq <- getLine
        putStrLn "Qual o afabeto em que a sequência será encriptada ?"
        putStrLn "OPÇÕES -> morse , binario, hexadecimal e octal, cifra de cesar "
        file <- getLine
        if file == (map toLower "cifra de cesar") then   
             do
              putStrLn "Qual a chave ?"
              c <- getLine 
              do
               let chave = read c :: Int
               putStrLn(encrypt (cifra_de_cesar chave) seq)
               else 
                   do
                    alf <- readFile (alphabets_file file)
                    let alfabeto = lines alf   
                    putStrLn (encrypt alfabeto seq)

       
decripitar :: IO()
decripitar = do
        system("clear")
        putStrLn "Qual o primeiro afabeto ?"
        file1 <- getLine
        putStrLn "Digite a sequência que será decodificada."
        putStrLn "OBS: Por favor digite caracteres pertencentes ao primeiro alfabeto (exemplo de não aceitos: ã, ~ , !,@ ,# ,.)."        
        seq <- getLine
        if file1 == (map toLower("natural")) then 
            do
              putStrLn "Qual o segundo afabeto ?"
              file2 <- getLine
              alf2 <- readFile (alphabets_file file2)
              let alfabeto2 = lines alf2     
              putStrLn (encrypt alfabeto2 seq)
              else if file1 == (map toLower "cifra de cesar") then
                   do
                    putStrLn "Qual a chave ?"
                    c <- getLine 
                    let chave = read c :: Int
                    putStrLn(encrypt (cifra_de_cesar (length(alf)-chave)) seq)
                    else
                        do
                         putStrLn "Qual o segundo afabeto ?"
                         file2 <- getLine
                         alf1 <- readFile (alphabets_file file1)
                         let alfabeto1 = lines alf1
                         alf2 <- readFile (alphabets_file file2)
                         let alfabeto2 = lines alf2
                         putStrLn (decrypt alfabeto1 alfabeto2 seq)

adicionar_alfabeto :: IO()
adicionar_alfabeto = do
         system("clear") 
         putStrLn "Digite o nome do arquivo em que o alfabeto será salvo."
         file <- getLine
         putStrLn "Por digite cada letra do alfabeto separada por um espaço."
         putStrLn "Caso não digite as 64 letras para o alfabeto, será auto completado com letra do alfabeto natural."
         alfabeto <- getLine
         add_aphabet file alfabeto
         putStrLn "Arquivo salvo."



menu_inicial :: IO(String)
menu_inicial = do 
        system("clear") 
        putStrLn "            Bem vindo ao encriptador básico version 1.2"
        putStrLn "                     Selecione uma opção "
        putStrLn "1.  Encriptar em um dado alfabeto (morse , binario, hexadecimal e octal )"
        putStrLn "2.  Desencriptar de um alfabeto 'a' para um alfabeto 'b'"
        putStrLn "3.  Criar um alfabeto"
        putStrLn "6. Sair"
        select <- getLine 
        if(select == "1") then 
            do
             encripitar
             getLine
             menu_inicial
             else if (select == "2") then
                 do 
                  decripitar
                  getLine
                  menu_inicial
                  else if(select == "3") then
                      do
                       adicionar_alfabeto
                       getLine
                       menu_inicial
                       else if(select == "6") then
                            return "Bye Bye"
                            else 
                               do
                                system("clear") 
                                putStrLn "Opção invalida " 
                                getLine
                                menu_inicial                  
