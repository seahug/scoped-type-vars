{-# LANGUAGE ExplicitForAll #-}

f :: forall a . [a] -> [a]
f xs = ys ++ ys
    where
        ys :: [a]
        ys = reverse xs

main = print [1, 2, 3, 4, 5]
