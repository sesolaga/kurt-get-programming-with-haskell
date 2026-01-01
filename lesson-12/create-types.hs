type FirstName = String

type MiddleName = String

type LastName = String

type Agt = Int

type Height = Int

data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName

showFirstName :: Name -> FirstName
showFirstName (Name f l) = f
showFirstName (NameWithMiddle f m l) = f

showLastName :: Name -> LastName
showLastName (Name f l) = l
showLastName (NameWithMiddle f m l) = l

data Sex = Male | Female

showSexInitial :: Sex -> String
showSexInitial Male = "M"
showSexInitial Female = "F"

showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"

data RhType = Pos | Neg

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

data ABOType = A | B | AB | O

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

data BloodType = BloodType ABOType RhType

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

-- W/out RhType handling
canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True -- Universal donor
canDonateTo _ (BloodType AB _) = True -- Universal receiver
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

data Patient = Patient
  { name :: Name,
    sex :: Sex,
    age :: Int,
    height :: Int,
    weight :: Int,
    bloodType :: BloodType
  }

showPatientName :: Patient -> String
showPatientName p = showFirstName (name p) ++ ", " ++ showLastName (name p)

jackieSmith :: Patient
jackieSmith =
  Patient
    { name = Name "Jackie" "Smith",
      age = 43,
      sex = Female,
      height = 62,
      weight = 115,
      bloodType = BloodType O Neg
    }

jackieSmithUpdated = jackieSmith {age = 44}

johnSmith =
  Patient
    { name = Name "Smith" "John",
      sex = Male,
      age = 46,
      height = 72,
      weight = 210,
      bloodType = BloodType AB Pos
    }

-- Homework

canPatientDonateTo :: Patient -> Patient -> Bool
canPatientDonateTo p1 p2 = canDonateTo (bloodType p1) (bloodType p2)

printLine :: String -> String
printLine s = s ++ "\n"

printKeyValue :: String -> String -> String
printKeyValue key value = printLine (key ++ ": " ++ value)

patientSummary :: Patient -> String
patientSummary p =
  printLine "***********"
    ++ printKeyValue "Patient Name" (showPatientName p)
    ++ printKeyValue "Sex" (showSex (sex p))
    ++ printKeyValue "Age" (show (age p))
    ++ printKeyValue "Height" (show (height p) ++ " in.")
    ++ printKeyValue "Weight" (show (weight p) ++ " lbs.")
    ++ printKeyValue "BloodType" (showBloodType (bloodType p))
    ++ printLine "***********"

toPrint = putStr (patientSummary johnSmith)