-- Typsynonym
type StringPair = (String, String)
-- Typinkapsling med deriverade typklassinstanser
newtype NamnTyp =  Namn (String, String) deriving (Eq, Ord, Show)

efternamn :: NamnTyp -> String
efternamn (Namn (_,y)) = y

-- Hade funkat för type NamnTyp = (String, String)
-- efternamn = snd

-- Enkel ny datatyp med värdekonstruktorer
data WeekEndDay = Friday | Saturday | Sunday deriving (Eq, Ord, Show)

-- Parametriserad, rekursiv datatyp
data List' a = EmptyList | ListItem a (List' a)

-- Datatyp med flera olika distinkta värdekonstruktorer
data UserType = NotAUser | User String Int | Uid Int | Username String

-- En instans av typklassen Show
instance Show UserType where
  show (NotAUser) = "Not a user"
  show (User x y) = x ++ " " ++ (show y)
  show (Uid x)  = show x
  show (Username x) = x

 --En ny typklass, se boken
--class YesNo a where
-- (yes) :: a -> True
--  (no) :: a -> False
 --Användningsexempel - Either String som typ för att skicka felmeddelanden
--mightGoWrong :: Int -> Boll -> Kaka -> Either String (Kex, Foo)


--case mightGoWrong x y z of
--  Left error -> printError error
--  Right (u, v) -> doSomething u v


