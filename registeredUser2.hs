module RegisteredUser where

newtype Username = UserName String

newtype AccountNumber = AccountNumber Integer

data User =
    UnregisteredUser
  | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = 
  putStrLn "UnregisteredUser"
printUser (RegisteredUser
            (UserName name)
            (AccountNumber acctNum)) =
  putStrLn $ name ++ " " ++ show acctNum
