-- | Write a function that determines whether its input list is a palindrome.

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []         = True
isPalindrome (x:[])     = True
isPalindrome (x:xs)     = x == last xs && isPalindrome (subList (x:xs))
    where subList (_:xs) = take (length xs - 1) xs     -- ^ subList returns all elements of a list except for first and last (same as 'init')
