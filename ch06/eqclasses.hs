class BasicEq a where 
    isEqual :: a -> a -> Bool
    
instance BasicEq Bool where
    isEqual True  True  = True
    isEqual False False = True
    isEqual _     _     = False

class BasicEq2 a where
    isEqual2, isNotEqual2 :: a -> a -> Bool

    isEqual2 x y = not (isNotEqual2 x y)
    isNotEqual2 x y = not (isEqual2 x y)
