module PrettyStub where 

data Doc = ToBeDefined 
           deriving (Show)

text :: String -> Doc
text s = undefined

double :: String -> Doc
double s = undefined

char :: Char -> Doc
char c = undefined

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

hsep :: [Doc] -> Doc
hsep xs = undefined

fsep :: [Doc] -> Doc
fsep xs = undefined
