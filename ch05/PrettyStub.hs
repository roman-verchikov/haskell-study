import SimpleJSON

data Doc = ToBeDefined 
           deriving (Show)

string :: String -> Doc
string s = undefined

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
