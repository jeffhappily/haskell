-- Reverse polish notation
rpn :: (Fractional a, Read a) => String -> a
rpn str = sec [] $ words str
    where
        sec (x:y:buf) ("+":xs) = sec (x + y : buf) xs
        sec (x:y:buf) ("-":xs) = sec (x - y : buf) xs
        sec (x:y:buf) ("*":xs) = sec (x * y : buf) xs
        sec (x:y:buf) ("/":xs) = sec (x / y : buf) xs
        sec (x:_) []           = x
        sec buf (x:xs)         = sec (read x : buf) xs
        sec [] []              = 0

main :: IO ()    -- This says that main is an IO action.
main = do
    putStrLn $ show $ rpn "2 2 5 2 8 5 + * * * *"
    return ()
