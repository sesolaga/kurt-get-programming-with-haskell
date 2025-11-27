{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

type Robot = forall r. ((String, Int, Int) -> r) -> r

cup flOz = \message -> message flOz

aCup = cup 6

coffeeCup = cup 12

getOz aCup = aCup (\flOz -> flOz)

drink aCup ozDrank =
  if ozDiff > 0
    then cup ozDiff
    else cup 0
  where
    ozDiff = flOz - ozDrank
    flOz = getOz aCup

isEmpty aCup = getOz aCup == 0

afterManySips = foldl drink coffeeCup [1, 1, 1, 1, 3]

left = getOz afterManySips

robot (name, attack, hp) = (\message -> message (name, attack, hp))

killerRobot = robot ("Kill3r", 25, 200)

name (n, _, _) = n

attack (_, a, _) = a

hp (_, _, hp) = hp

getName aRobot = aRobot name

getAttack aRobot = aRobot attack

getHp aRobot = aRobot hp

setName aRobot newName = robot (newName, getAttack aRobot, getHp aRobot)

setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot (n, newAttack, h))

setHp aRobot newHp = aRobot (\(n, a, h) -> robot (n, a, newHp))

printRobot aRobot = aRobot (\(n, a, h) -> n ++ " attack: " ++ (show a) ++ " hp: " ++ (show h))

damage aRobot attackDamage = aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

fight aRobot defender = damage defender attack
  where
    attack =
      if getHp aRobot > 0
        then getAttack aRobot
        else 0

gentleGiant = robot ("Mr. Friendly", 10, 300)

gentleGiantRound1 = fight killerRobot gentleGiant

killerRobotRound1 = fight gentleGiant killerRobot

gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1

killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1

gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2

killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2

res1 = printRobot gentleGiantRound3

res2 = printRobot killerRobotRound3

-- Homework 1

printRobots xs = map (\aRobot -> aRobot (\(n, a, h) -> h)) xs

robocop = robot ("Robotcop", 35, 500)

term = robot ("Terminator", 50, 1000)
l = [killerRobot, gentleGiant, term]

-- Homework 3
fightTerm aRobot = fight aRobot term

battleResult = map fightTerm l

termHpLeftInDifferentFights = map getHp battleResult


-- Homework 2
-- threeRoundFight x y =
--   ( \xA3 yA3 ->
--       if getHp xA3 > getHp yA3
--         then xA3
--         else yA3
--   )
--     (\x y -> fight x y) ((\x y -> fight x y) (\x y -> fight y x))
--     (\x y -> fight y x)

-- Source - https://stackoverflow.com/a/68508024
-- Posted by amalloy, modified by community. See post 'Timeline' for change history
-- Retrieved 2025-11-26, License - CC BY-SA 4.0

oneRound :: (Robot, Robot) -> (Robot, Robot)
oneRound (a, b) = let damagedb = fight a b
                  in (fight damagedb a, damagedb)

threeRoundFight :: Robot -> Robot -> (Robot, Robot)
threeRoundFight a b = oneRound (oneRound (oneRound (a, b)))

z = threeRoundFight term robocop
termHpLeft = getHp (fst z)
robocopHpLeft = getHp (snd z)