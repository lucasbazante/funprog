module CustomType where

data Weekday = Mon 
             | Tue 
             | Wed 
             | Thu 
             | Fri 
             | Sat 
             | Sun
             deriving ( Enum )

instance (Eq Weekday) where
   Mon == Mon = True
   Tue == Tue = True
   Wed == Wed = True
   Thu == Thu = True
   Fri == Fri = True
   Sat == Sat = True
   Sun == Sun = True
   _   == _   = False

instance (Show Weekday) where
   show Mon = "Monday"
   show Tue = "Tuesday"
   show Wed = "Wednesday"
   show Thu = "Thursday"
   show Fri = "Friday"
   show Sat = "Saturday"
   show Sun = "Sunday"

nextDay :: Weekday -> Weekday
nextDay day | day == Mon = Tue
            | day == Tue = Wed
            | day == Wed = Thu
            | day == Thu = Fri
            | day == Fri = Sat
            | day == Sat = Sun
            | day == Sun = Mon

nextWorkingDay :: Weekday -> Weekday
nextWorkingDay day | day == Fri = Mon
                   | day == Sat = Mon
                   | day == Sun = Mon
                   | otherwise  = nextDay day
