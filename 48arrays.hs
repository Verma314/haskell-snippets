import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST


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
updatedArray = beansInBuckets // [(1,9),(3,11)]



-- to 100 to each element in our UArray
newArrya1000 = accum (+) updatedArray $ zip [0 .. 3] $ cycle [1000]




-----------------------------------
-----------------------------------
-----------------------------------
--  STUArray --

listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
    let end =  length vals - 1
    myArray <- newArray (0,end) 0
    forM_ [0 .. end] $ \i -> do             
      let val = vals !! i                   
      writeArray myArray i val              
    return myArray

-- to pull these STUArrays out of ST context, use:
listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ listToSTUArray vals


listToUArray_ :: [Int] -> UArray Int Int
listToUArray_ vals = runSTUArray $ do
    let end =  length vals - 1
    myArray <- newArray (0,end) 0
    forM_ [0 .. end] $ \i -> do
        let val = vals !! i
        writeArray myArray i val
    return myArray


{-
swapST :: (Int,Int) -> (Int,Int)
swapST (x,y) = runST $ do
   x' <- newSTRef x
   y' <- newSTRef y
   writeSTRef x' y
   writeSTRef y' x
   xfinal <- readSTRef x'
   yfinal <- readSTRef y'
   return (xfinal,yfinal)

-}
-- my bubble sort:
bubbleSort :: [Int] -> UArray Int Int
bubbleSort vals = runSTUArray $  do
                  let end =  length vals - 1
                  stuArray <- listToSTUArray vals
                  forM_ [0..end] $ \ j -> do 
                        forM_ [0..(end-1)] $  \ i -> do 
                            val1 <- readArray stuArray i
                            val2 <- readArray stuArray (i+1)
                            if (val1 > val2) then 
                                do
                                writeArray stuArray i val2
                                writeArray stuArray (i+1) val1
                            else return ()    
                  return stuArray  


-- author's implementation
bubbleSort2 :: UArray Int Int -> UArray Int Int
bubbleSort2 myArray = runSTUArray $ do
   stArray <- thaw myArray                             
   let end = (snd . bounds) myArray                    
   forM_ [1 .. end] $ \i -> do
     forM_ [0 .. (end - i)] $ \j -> do
       val <- readArray stArray j                      
       nextVal <- readArray stArray (j + 1)
       let outOfOrder = val > nextVal
       when outOfOrder $ do                            
         writeArray stArray j nextVal
         writeArray stArray (j + 1) val
   return stArray                                                                    
-- they are using ```thaw``` so that they can treat the UArray like a STUArray
-- (snd. bounds) to get the upper limit

-- exercises,

-- 42.1 crossover
crossover :: UArray Int Int -> UArray Int Int -> Int -> UArray Int Int
crossover myArray1 myArray2 pt = runSTUArray $ do 
                                stuArray1 <- thaw myArray1  :: ST s (STUArray s Int Int)
                                stuArray2 <- thaw myArray2  :: ST s (STUArray s Int Int)
                                let end = (snd . bounds) myArray1
                                myArray <- newArray (0, end) 0 
                                forM_ [0..end] $ \ i -> do
                                                        value1 <- readArray stuArray1 i
                                                        value2 <- readArray stuArray2 i
                                                        if i < pt then
                                                            writeArray myArray i  value1
                                                        else 
                                                            writeArray myArray i value2
                                return myArray                                                                                                                  






--Write a function that takes a UArray Int Int as an input.
--  The input will have a mixture of zeros and other values. 
-- The function, replaceZeros, should return the array with all of the zeros replaced with the value –1.
modifyArr :: UArray Int Int -> UArray Int Int
modifyArr myArray = runSTUArray $ do
                        stuArray <- thaw myArray :: ST s (STUArray s Int Int)
                        let end = (snd . bounds) myArray
                        forM_ [0..end] $ \ i -> do 
                                                val <- readArray stuArray i 
                                                if (val == 0 )
                                                then writeArray stuArray i (-1)
                                                else return ()
                        return stuArray                                                
