module Analysis where

data Criticality = Maximum | Minimum | Inflection
    deriving (Eq, Show, Read)

data Extremum x y = Extremum
    { exAbscissa :: x
    , exOrdinate :: y
    , exType     :: Criticality
    } deriving (Eq, Show)

extremum :: (Fractional t, Ord t) =>
    (t -> t) -> (t -> t) -> (t -> t) -> t -> (t, t) -> Extremum t t
extremum f f' f'' e r = Extremum x y t
  where x = solve f' f'' e r
        y = f x
        c = f'' x
        t | c > e = Minimum
          | c < (-e) = Maximum
          | otherwise = Inflection

-- TODO: use bisection to ensure bounds
solve :: (Fractional t, Ord t) =>
     (t -> t) -> (t -> t) -> t -> (t, t) -> t
solve f f' e (x0, x1) = head . convergedBy e . iterate step $ x0
  where step x = x - f x / f' x

dropWhile2 :: (t -> t -> Bool) -> [t] -> [t]
dropWhile2 p xs@(x : xs'@(x' : _)) = if not (p x x') then xs else dropWhile2 p xs'
dropWhile2 _ any = any

convergedBy :: (Num t, Ord t) => t -> [t] -> [t]
convergedBy e = dropWhile2 unconverging
  where unconverging x x' = abs (x - x') >= e
