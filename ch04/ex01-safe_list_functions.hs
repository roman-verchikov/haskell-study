-- Write your own “safe” definitions of the standard partial list functions,
-- but make sure that yours never fail. As a hint, you might want to consider
-- using the following types.  
--
-- safeHead :: [a] -> Maybe a
-- safeTail :: [a] -> Maybe [a]
-- safeLast :: [a] -> Maybe a
-- safeInit :: [a] -> Maybe [a]

safeHead :: [a] -> Maybe a
safeHead (a:_) = Just a
safeHead []    = Nothing


safeTail :: [a] -> Maybe [a]
safeTail (_:rest) = Just rest
safeTail []       = Nothing


safeLast :: [a] -> Maybe a
safeLast []       = Nothing
safeLast (a:[])   = Just a
safeLast (_:rest) = safeLast rest


safeInit :: [a] -> Maybe [a]
safeInit []     = Nothing
safeInit (x:xs) = Just (take (length xs - 1) xs)
