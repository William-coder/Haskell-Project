{-|

Welcome to GDB Online.
GDB online is an online compiler and debugger tool for C, C++, Python, Java, PHP, Ruby, Perl,
C#, OCaml, VB, Swift, Pascal, Fortran, Haskell, Objective-C, Assembly, HTML, CSS, JS, SQLite, Prolog.
Code, Compile, Run and Debug online from anywhere in world.

-}
 
checkOnes :: String -> Bool
checkOnes str = filter (/='1') str == ""
bitStuffing :: String -> Int -> String
bitStuffing "" _  = []
bitStuffing bss n
  | head bss == '0' = "0" ++ bitStuffing (tail bss) n
  | otherwise = if length bss >= n && checkOnes (take n bss) then (take n bss) ++ "0" ++ bitStuffing (drop n bss) n
                else "1" ++ bitStuffing (tail bss) n

deStuffing :: String -> Int -> String
deStuffing "" _ = []
deStuffing bss n
  | head bss == '0' = "0" ++ deStuffing (tail bss) n
  | otherwise = if length bss >= n && checkOnes (take n bss) then (take n bss) ++ deStuffing (drop (n+1) bss) n
                else "1" ++ deStuffing (tail bss) n
 
--main = print(drop 3 "abcdefg")
--main = print(bitStuffing "00111110111110111" 5)