module SportsPlayers where

data Sport = PingPong | Hockey | Soccer

data Player = Player Sport

isSoccerPlayer :: Player -> Bool
isSoccerPlayer (Player Soccer) = True
isSoccerPlayer _             = False
