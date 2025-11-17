import Data.List

names = [("Ian", "Curtis"), ("Bernard", "Sumner"), ("Peter", "Hook"), ("Stephen", "Morris"), ("Alex", "Sumner"), ("Vincent", "Sumner"), ("Greg", "Sumner"), ("Sergei", "Hook"), ("Alex", "Hook")]

sortedNames = sort names

sortedByLastName = sortBy (\n1 n2 -> if snd n1 > snd n2 then GT else if snd n1 < snd n2 then LT else EQ) names

compareNames name1 name2 =
  if lastName1 > lastName2
    then GT
    else
      if lastName1 < lastName2
        then LT
        else compareFirstNames name1 name2
  where
    lastName1 = snd name1
    lastName2 = snd name2
    compareFirstNames name1 name2 =
      if firstName1 > firstName2
        then GT
        else
          if firstName1 < firstName2
            then LT
            else EQ
      where
        firstName1 = fst name1
        firstName2 = fst name2

compareNamesPart name1 name2 namePartGetter =
  if namePart1 > namePart2
    then GT
    else
      if namePart1 < namePart2
        then LT
        else EQ
  where
    namePart1 = namePartGetter name1
    namePart2 = namePartGetter name2

compareNamesImproved name1 name2 =
  if compareLastNamesResult == EQ
    then compareNamesPart name1 name2 fst
    else compareLastNamesResult
  where
    compareLastNamesResult = compareNamesPart name1 name2 snd

compareNamesUsingCompare name1 name2 =
  if compareLastNamesResult == EQ
    then compare (fst name1) (fst name2)
    else compareLastNamesResult
  where
    compareLastNamesResult = compare (snd name1) (snd name2)