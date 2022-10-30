{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module ExMatrix2x2
    ( matrix
    , zero
    , identity
    , rows
    , cols
    , getElem
    , transpose
    , det
    , isDiagonal
    , isTriangular
    , isLowerTriangular
    , isUpperTriangular
    , singular
    , invertible
    , inverse
    ) where

type Number = Double
type Row = [Number]
type Col = [Number]

data Matrix2x2 = Matrix2x2 { a :: Number, -- 1 1
                             b :: Number, -- 2 1
                             c :: Number, -- 1 2
                             d :: Number  -- 2 2
                           }

instance Show Matrix2x2 where
    show Matrix2x2{..} = tuple a c ++ "\n" ++ tuple b d
        where
        tuple i j = "( " ++ show i ++ ", " ++ show j ++ " )"

instance Eq Matrix2x2 where
    Matrix2x2{..} == (Matrix2x2 a' b' c' d')  =  a == a'
                                              && b == b'
                                              && c == c'
                                              && d == d'

instance Num Matrix2x2 where
    Matrix2x2{..} + (Matrix2x2 a' b' c' d') = matrix (a + a') (b + b') (c + c') (d + d')
    
    Matrix2x2{..} * (Matrix2x2 a' b' c' d') = matrix (a * a' + c * b') (b * a' + d * b') (a * c' + c * d') (b * c' + d * d')
    
    negate Matrix2x2{..} = matrix (-a) (-b) (-c) (-d)
    
    abs Matrix2x2{..} = matrix (abs a) (abs b) (abs c) (abs d)
    
    signum Matrix2x2{..} = matrix (signum a) (signum b) (signum c) (signum d)
    
    fromInteger n = matrix (fromIntegral n) (fromIntegral n) (fromIntegral n) (fromIntegral n)

-- matrix a b c d should create the matrix
-- ( a c )
-- ( b d )
matrix :: Number -> Number -> Number -> Number -> Matrix2x2
matrix a b c d = Matrix2x2 { a = a, b = b, c = c, d = d }

zero :: Matrix2x2
zero = matrix 0 0 0 0

identity :: Matrix2x2
identity = matrix 1 0 0 1

rows :: Matrix2x2 -> [Row]
rows Matrix2x2{..} = [[a, c], [b, d]]

cols :: Matrix2x2 -> [Col]
cols Matrix2x2{..} = [[a, b], [c, d]]

getElem :: (Int,Int) -> Matrix2x2 -> Number
getElem (i, j) mx = (!! j) $ (!! i) $ rows mx

transpose :: Matrix2x2 -> Matrix2x2
transpose Matrix2x2{..} = matrix a c b d

det :: Matrix2x2 -> Number
det Matrix2x2{..} = a * d - b * c

isDiagonal :: Matrix2x2 -> Bool
isDiagonal Matrix2x2{..} =  a == 1
                         && b == 0
                         && c == 0
                         && d == 1

isTriangular :: Matrix2x2 -> Bool
isTriangular mx = isLowerTriangular mx || isUpperTriangular mx

isLowerTriangular :: Matrix2x2 -> Bool
isLowerTriangular Matrix2x2{..} = c == 0

isUpperTriangular :: Matrix2x2 -> Bool
isUpperTriangular Matrix2x2{..} = b == 0

singular :: Matrix2x2 -> Bool
singular = (== 0) . det

invertible :: Matrix2x2 -> Bool
invertible = not . singular

inverse :: Matrix2x2 -> Matrix2x2
inverse mx@Matrix2x2{..} = matrix (d * dt) ((-b) * dt) ((-c) * dt) (a * dt)
    where dt = (1 / (det mx))
