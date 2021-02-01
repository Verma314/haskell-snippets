import Data.Array.Unboxed

zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0,5) [(1,True)]

oneIndexArray :: Int ->  UArray Int Int
oneIndexArray up = array (1,up) $ zip [1..up] [1..up]

test1 = oneIndexArray 5


{-
Create an array with the following signature:

qcArray :: UArray Int Bool
The array contains five elements indexed at 0 and the 2,
 and three elements are set to True.

-}

qcArray :: UArray Int Bool
qcArray = array (0,5) [(2,True),(3,True)]


---------
---------
-- Modifying elements:

beansInBuckets :: UArray Int Int
beansInBuckets = array (0,3) [] -- initializes everything to zero


-- we  use the  // operator to update values in a UArray
newArray = beansInBuckets // [(1,9),(3,11)]



-- to 100 to each element in our UArray
newArrya1000 = accum (+) newArray $ zip [0 .. 3] $ cycle [1000]