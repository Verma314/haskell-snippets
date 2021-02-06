{-
This need for a way to read and write state is common enough in Haskell programs 
that the standard libraries provide a monad named State that is dedicated to this purpose. 

This monad lives in the Control.Monad.State module. 

In the previous hs file,
our Parse type carried around a ByteString as its piece of state,
the State monad can carry any type of state. We'll refer to the state's unknown type as s.

What's an obvious and general thing we might want to do with a state?
Given a state value, we inspect it, then produce a result and a new state value.

Let's say the result can be of any type a. 
A type signature that captures this idea is 
s -> (a, s)
 take a state s, do something with it, and return a result a and possibly a new state s.
-}

-- creating our own State type,
type SimpleState s a = s -> (a, s)
-- this means all the functions of type  s -> (a, s) are of type SimpleState s a,
-- this function takes a state and returns (a,s)
x :: SimpleState String Int
x myStr = (100,myStr) 

-- but monad has a type constructor with a single type variable,
type StringState a = SimpleState String a
x2x :: StringState Int
x2x myStr = (100,myStr++ "hi!") 
-- you cant pass x, x2 ints etc, these just take a string
-- if you wanna pass int, you have to create a type synonum of SimpleState Int a

-- all functions with the type s -> (a,s) where s is a string (ie inputs a string)
   -- are of the type StringState a
-- like myString -> (1,myString ++ "1") is of the type ```StringState Int```

--  understand that we can partially apply a type just as we can partially apply a normal function
--  so, we've bound the type variable s to String

-- The next ingredient we need to make a monad is a definition for the return function:
returnSt :: a -> SimpleState s a -- ie something that puts the "value" back into the monad (the context)
returnSt a = \s -> (a, s)

-- example,
-- put 10 in the result
x3 = returnSt 10
-- then put "hi" in the result
x3state = x3 "hi"

-- or 
x4 = returnSt 10 "HI"
-- x4 == (10,"HI")  which is precisely our state object, ie it put the values into the orginal "Context"
{- Alternate,
returnAlt :: a -> SimpleState s a
returnAlt a s = (a, s),
-}



-- to implement bind, means  a function like this:
--myBind :: (SimpleState s a) -> (a -> SimpleState s b) -> (SimpleState s b)
-- it extracts out the result from the first monad,
-- applies it to the 2nd function (2nd argument), and "returns" the result
{-
myBind firstMonad secondFucntion = \ state  -> let (value, newState) =  secondFucntion state
    in  (secondFucntion value) newState
-}
-- or    
{-
bindSt m k = \ s -> let (a, s') = m s 
                    in (k a) s' --wtf?    

bindSt m k = (k a) s'
    where (a,s') = m s
-}

 -- (result,state) -> (result -> (result2, state2) ) -> (result2,state2)




myBindSt :: (SimpleState s a) -> (a -> SimpleState s b) -> SimpleState s b
myBindSt x1 x2 inputState =  let (resultOfx1, stateAfterx1) = x1 inputState -- see x1 will give us a normal tuple on applying
                             in  -- x2 takesin the result of x1
                             x2 resultOfx1 stateAfterx1 -- = \  inputState -> ( resultOfx1 )


-- explanation
-- let,
x1 str = (1,str) 
-- x2 is a func that should take the result value out of x1, and then apply a string on it, and then generate the (a,s)
x2 num = \ str -> (num,str)

-- ideally wanted behavior of bind:
smallBind =  x2 (fst (x1 "nope")) "yes" -- it takes the value ```1``` out of x1, gives it to x2, which becomes SimpleState s a, and can now accept 
-- (1,"yes")



{-
>myBindSt x1 x2 "hi"
(1,"hi")

> :t myBindSt x1 x2
myBindSt x1 x2 :: Num b => SimpleState s b

-}

-- finally implementing bind >>=   (in the book)
bindSt :: (SimpleState s a) -> (a -> SimpleState s b) -> SimpleState s b
bindSt m k = \ s -> let (a, s') = m s 
                    in (k a) s' --wtf?        

-- or,
bindAlt step makeStep oldState =
    let (result, newState) = step oldState
    in  (makeStep result) newState



getSt :: SimpleState s s
getSt = \s -> (s, s)

putSt :: s -> SimpleState s ()
putSt s = \_ -> ((), s)



