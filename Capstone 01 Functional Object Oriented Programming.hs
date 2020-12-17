----- create a cup "object"

-- cup fl0z = (\ _ -> fl0z)


-- constructor:
cup fl0z = \ message -> message fl0z


-- create objects:
aCup = cup 6
coffeeCup = cup 12

-- create messages:
-- NOTE that in Haskell, messages and objects interact liket this <message> <object>
-- get the number of Oz from an object

getOz aCup = aCup (\x -> x)


-- drinkFromCup oz aCup = cup (aCup ( \ x ->  x - oz ) )

drinkFromCup aCup ozDrank = if (fl0z - ozDrank >= 0) then cup ( fl0z - ozDrank)
                            else cup 0
                            where fl0z = getOz aCup



-- another message
isCupEmpty aCup = getOz aCup == 0


afterOneSip = drinkFromCup coffeeCup 1
afterTwoSips = drinkFromCup afterOneSip 1
afterGulp = drinkFromCup afterTwoSips 4

-- multiple sips:
leftoverCup = foldl drinkFromCup coffeeCup [ 1,1,1]

multipleSips myCup sips = foldl drinkFromCup myCup sips

-------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------

-- Robots:

-- constructor:
robot (name,attack,hp) = \message -> message (name,attack,hp)


-- creating an object
killerRobot = robot ("Killer",100,100) 

-- getters 
getName robo = robo (\ (name,_,_) -> name)
getAttack robo = robo  (\ (_,attack,_) -> attack)
getHp robo = robo  (\ (_,_,hp) -> hp)


-- setters
setName robo newName = robot (newName, getAttack robo, getHp robo)
-- alternate
setName2 robo newName = robo ( \(n,a,h) -> robot (newName,a,h))

setAttack robo newAttack = robot (getName robo , newAttack , getHp robo )
setHp robo newHp = robot ( getName robo, getAttack robo, newHp)

--

-- damage robot:
damage robo hpDamage = robo ( \(name,attack,hp) -> robot ( name, attack, hp - hpDamage) ) 
--damage aRobot attackDamage = aRobot (\(n,a,h) -> robot (n,a,h-attackDamage))

-- fighting:

fight aRobot defender = damage defender attack
  where attack = if getHp aRobot > 10
                 then getAttack aRobot
                 else 0

gentleGiant = robot ("Mr. Friendly", 10, 300)


fastRobot = robot ("speedy", 15, 40)
slowRobot = robot ("slowpoke",20,30)

{-
slowRobotRound1 = fight fastRobot slowRobot
fastRobotRound1 = fight slowRobotRound1 fastRobot
slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
slowRobotRound3 = fight fastRobotRound1 slowRobotRound2
slowRobotRound4 = fight fastRobotRound1 slowRobotRound3
fastRobotRound2 = fight slowRobotRound4 fastRobotRound1
-}

slowRobotRound1 = fight fastRobot slowRobot
fastRobotRound1 = fight slowRobotRound1 fastRobot
slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
fastRobotRound2 = fight fastRobotRound1 slowRobotRound1
slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
fastRobotRound3 = fight slowRobotRound3 fastRobotRound2

-- logical error above (proabably)