data Hunbool = Igaz | Hamis

instance Eq Hunbool where
        Igaz == Hamis = False
        Hamis == Igaz = False
        Hamis == Hamis = True
        Igaz == Igaz   = True

instance Ord Hunbool where
        Igaz <= Hamis = False
        x <= y        = True

instance Show Hunbool where
        show Igaz = "Igaz"
        show Hamis = "Hamis"

instance Enum Hunbool where
        fromEnum Hamis = 0
        fromEnum Igaz  = 1
        toEnum      1  = Igaz
        toEnum      0  = Hamis

instance Bounded Hunbool where
        minBound = Hamis
        maxBound = Igaz

type MagyarBool = Hunbool
newtype Hunboollist = H [Hunbool] deriving Show

instance Bounded Hunboollist where
        minBound = H []

data Luislist a = Sep a (Luislist a) | E
instance Show a => Show (Luislist a) where
        show E          = "<>"
        show (Sep x E)  = "<" ++ show x ++ ">"
        show (Sep x xs) = "<" ++ show x ++ ","
                          ++ tail (show xs)

myCoolList :: Luislist Integer
myCoolList = Sep 1 (Sep 2 (Sep 3 E))
myEmptylist= E
presList = Sep "Obama" (Sep "Bush" (Sep "Clinton" E))
metaList = [luisinit presList, luistail presList]

luistail :: Luislist a -> Luislist a
luistail (Sep x xs) = xs
luisinit :: Luislist a -> Luislist a
luisinit (Sep x E) = E
luisinit (Sep x xs) = Sep x (luisinit xs)

foxnewsMexicancountries :: Luislist String
foxnewsMexicancountries = Sep "Salvador" (Sep "Honduras" (Sep "Nicaragua" E))

instance Functor Luislist where
  fmap f E          = E
  fmap f (Sep x xs) = Sep (f x) (fmap f xs)


  data Weekday =  Monday | Tuesday | Wednesday | Thursday |
                  Friday | Saturday| Sunday
                  deriving (Read, Eq, Ord)
  instance Enum Weekday where
    succ Monday     = Tuesday
    succ Tuesday    = Wednesday
    succ Wednesday  = Thursday
    succ Thursday   = Friday
    succ Friday     = Saturday
    succ Saturday   = Sunday
    succ Sunday     = Monday

  instance Show Weekday where
    show Monday     = "Lunes"
    show Tuesday    = "Martes"
    show Wednesday  = "Miercoles"
    show Thursday   = "Jueves"
    show Friday     = "Viernes"
    show Saturday   = "Sabado"
    show Sunday     = "Domingo"
