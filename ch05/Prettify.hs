-- | Exports helper functions for JSON pretty printer library ("PrettyJSON"), 
-- described in chapter 5 of the "Real world Haskell" book.
module Prettify 
(
    Doc(),
    empty,
    char,
    text,
    double,
    line,
    (<>),
    (</>),
    hsep,
    fsep,
    hcat,
    compact,
    pretty,
    fits
) where

-- | An abstraction above possible JSON values (bool, string, number, array and structs).
data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)

-- | Represents empty value as 'Doc'.
empty :: Doc
empty = Empty

-- | Represents char value as 'Doc'.
char :: Char -> Doc
char c = Char c

-- | Represents 'String' value (text) as 'Doc'.
text :: String -> Doc
text "" = Empty
text s  = Text s

-- | Represents number (either integer or floating point) as 'Doc'.
double :: Double -> Doc
double d = text (show d)

-- | Line is a combination of other primitives ('char', 'double' and 'text') as 'Doc'.
line :: Doc
line = Line

-- | Concatenates 'Doc' primitives.
(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

-- | Concatenates list of 'Doc's.
hcat :: [Doc] -> Doc
hcat = fold (<>)

-- | TBD
fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

-- | TBD
hsep :: [Doc] -> Doc
hsep = fold (</>)

-- | TBD
(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

fsep :: [Doc] -> Doc
fsep = fold (</>)

compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) = 
              case d of 
                  Empty         -> transform ds
                  Char c        -> c : transform ds
                  Text s        -> s ++ transform ds
                  Line          -> '\n' : transform ds
                  a `Concat` b  -> transform (a:b:ds)
                  _ `Union` b   -> transform (b:ds)


pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) = 
                case d of 
                    Empty           -> best col ds
                    Char c          -> c : best (col + 1) ds
                    Text s          -> s ++ best (col + length s) ds
                    Line            -> '\n' : best 0 ds
                    a `Concat` b    -> best col (a:b:ds)
                    a `Union` b     -> nicest col (best col (a:ds))
                                                  (best col (b:ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                      where least = min width col



fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w-1) `fits` cs
