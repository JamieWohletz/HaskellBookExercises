data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show, Enum)

data ProgLang =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show, Enum)

data Programmer =
  Programmer { os :: OperatingSystem, lang :: ProgLang }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = enumFrom GnuPlusLinux

allLanguages :: [ProgLang]
allLanguages = enumFrom Haskell

-- This should have a length of 16.
allProgrammers :: [Programmer]
allProgrammers = [ Programmer os lang | os <- allOperatingSystems, lang <- allLanguages ]
