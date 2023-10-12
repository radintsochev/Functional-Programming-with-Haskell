import Data.Char (isSpace)

solve :: String -> Integer
solve = snd . flip eval id . filter (not . isSpace)

eval :: (Fractional a, Read a) => String -> (a -> a) -> (String, a)
eval str accum | [(num, rest)] <- readsPrec 0 str = oper rest (accum num)
eval ('(':str) accum = oper rest (accum num) where (')':rest, num) = eval str id
eval str accum = (str, accum 0) -- Default case when no number or parentheses is found

oper :: (Fractional a, Read a) => String -> a -> (String, a)
oper ('+':str) num = eval str (num +)
oper ('-':str) num = eval str (num -)
oper str num = (str, num)