module Alphabet where
import Data.Char
import Crypt_decrypt(split) 
--ALFABETO - base
base = " 141 142 143 144 145 146 147 150 151 152 153 154 155 156 157 160 161 162 163 164 165 166 167 170 171 172 101 102 103 104 105 106 107 110 111 112 113 114 115 116 117 120 121 122 123 124 125 126 127 130 131 132 061 062 063 064 065 066 067 070 071 060 "


alphabets_file :: String -> FilePath
alphabets_file selection  = "alfabetos/"++(map toLower(selection))++".txt"
 

--ADICIONAR ALFABETOS			 
swap :: String -> String
swap [] = []
swap list1 = " " ++ list1 ++ " " ++ drop (length list1) base 



add_aphabet :: String -> String -> IO()
add_aphabet arquivo alfabeto = writeFile ("alfabetos/"++arquivo++".txt") (lista_In_String(split ' ' ((swap alfabeto)++" ")))


lista_In_String :: [String] -> String
lista_In_String [] = []
lista_In_String (x:xs) =  x ++ "\n" ++ lista_In_String xs  





 {-
 morse = [" "," .-"," -..."," -.-."," -.."," ."," ..-."," --."," ...."," .."," .---"," -.-"," .-.."," --"," -."," ---"," .--."," --.-"," .-."," ..."," -"," ..-"," ...-"," .--"," -..-"," -.--"," --.."," .-"," -..."," -.-."," -.."," ."," ..-."," --."," ...."," .."," .---"," -.-"," .-.."," --"," -."," ---"," .--."," --.-"," .-."," ..."," -"," ..-"," ...-"," .--"," -..-"," -.--"," --.."," .----"," ..---"," ...--"," ....-"," ....."," -...."," --..."," ---.."," ----."," -----"," /","\n",""]
 binario = [" 01100001"," 01100010"," 01100011"," 01100100"," 01100101"," 01100110"," 01100111"," 01101000"," 01101001"," 01101010"," 01101011"," 01101100"," 01101101"," 01101110"," 01101111"," 01110000"," 01110001"," 01110010"," 01110011"," 01110100"," 01110101"," 01110110"," 01110111"," 01111000"," 01111001"," 01111010"," 01000001"," 01000010"," 01000011"," 01000100"," 01000101"," 01000110"," 01000111"," 01001000"," 01001001"," 01001010"," 01001011"," 01001100"," 01001101"," 01001110"," 01001111"," 01010000"," 01010001"," 01010010"," 01010011"," 01010100"," 01010101"," 01010110"," 01010111"," 01011000"," 01011001"," 01011010"," 00110001"," 00110010"," 00110011"," 00110100"," 00110101"," 00110110"," 00110111"," 00111000"," 00111001"," 00110000"," 00100000","\n",""]
 hexadecimal = [" 61"," 62"," 63"," 64"," 65"," 66"," 67"," 68"," 69"," 6a"," 6b"," 6c"," 6d"," 6e"," 6f"," 70"," 71"," 72"," 73"," 74"," 75"," 76"," 77"," 78"," 79"," 7a"," 41"," 42"," 43"," 44"," 45"," 46"," 47"," 48"," 49"," 4a"," 4b"," 4c"," 4d"," 4e"," 4f"," 50"," 51"," 52"," 53"," 54"," 55"," 56"," 57"," 58"," 59"," 5a"," 31"," 32"," 33"," 34"," 35"," 36"," 37"," 38"," 39"," 30"," 20","\n",""]
 octal = [" 141"," 142"," 143"," 144"," 145"," 146"," 147"," 150"," 151"," 152"," 153"," 154"," 155"," 156"," 157"," 160"," 161"," 162"," 163"," 164"," 165"," 166"," 167"," 170"," 171"," 172"," 101"," 102"," 103"," 104"," 105"," 106"," 107"," 110"," 111"," 112"," 113"," 114"," 115"," 116"," 117"," 120"," 121"," 122"," 123"," 124"," 125"," 126"," 127"," 130"," 131"," 132"," 061"," 062"," 063"," 064"," 065"," 066"," 067"," 070"," 071"," 060"," 040","\n",""]
-}
