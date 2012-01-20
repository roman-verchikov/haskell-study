-- Implementing simple JSON library in Haskell
--

module SimpleJSON
(
    JValue(..),
    getString,
    getInt,
    getDouble,
    getObject,
    getBool,
    isNull
) where 

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble (JNumber n) = Just n
getDouble _           = Nothing

getObject (JObject o) = Just o
getObject _           = Nothing

getBool (JBool b) = Just b
getBool _         = Nothing

isNull v = v == JNull
