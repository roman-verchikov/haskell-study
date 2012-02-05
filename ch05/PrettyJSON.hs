renderValue :: JValue -> Doc
renderValue (JBool true)  = text "true"
renderValue (JBool false) = text "false"
renderValue (JNull)       = text "null"
renderValue (JNumber num) = double num
renderValue (JString str) = string str


string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right d = char left <> x <> char right

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscape of
              Just r -> text r
              Nothing | mustEscape c = hexEscape c
                      | otherwise    = char c
    where mustEscape c = c == ' ' || c == '\x7f' || c > '\xff'

simpleEscape :: [(Char, String)]
simpleEscape = zipWith f "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where f a b = (a, ['\\', b])

smallHex :: Int -> Doc
smallHex x = text "\\u"
          <> text (replicate (4 - length h) '0')
          <> text h
    where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex c
            | otherwise   = astral (d - 0x10000)
    where d = ord c

series :: Char -> Char -> (a->Doc) -> [a] -> Doc
series open close item = enclose open close 
                       . fsep . punctuate (char ',') . map item
