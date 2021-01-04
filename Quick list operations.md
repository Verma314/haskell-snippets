#### Quick operations


0. Cartesian Product over two lists / List Comprehension
```
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
```

1. Retrieve Values
```
test9 = (!!) "1234567890" 1
```
2. Length
```
length [1,2,3,4,5]
```
3. Reverse
```
reverse [1,2,3,4,5]
```
4. Check if element in list
```
> elem 'p' "piry3u4233"
> 'e' `elem` "qwerty"
```

5. Retrieve n values
```
test7 = take 5 [1,2..100]
test8 = take 1000 [1,2..10]
```

6. Drop n values from the beginning
```
test9 = drop 5 [1,2,3,4,5,6,7,8,9]
```

7. Zip two lists into tuple pairs
```
> zip [1,2,3] [4,5,6]
[(1,4),(2,5),(3,6)]
```

8. Repeat the elements inside a list indefinitely 
```
take  10 (cycle [1,2,3])
```
9. Select elements between ```start``` to ```end``` from a list
```
subseq start end list =  drop start (take end list) 
```


10. How to split a string on '\n'
```
> lines "aa\nbb\nbb"
["aa","bb","bb"]
```

* more info on
```
look up package: Data.List
```

