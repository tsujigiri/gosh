data X = A | B | C | D | E | F | G | H | J | K | L | M | N | O | P | Q | R | S | T deriving Show
data Y = I1 | I2 | I3 | I4 | I5 | I6 | I7 | I8 | I9 | I10 | I11 | I12 | I13 | I14 | I15 | I16 | I17 | I18 | I19 deriving Show
data Move = Move X Y deriving Show
data Game = Game [Move] deriving Show

new_game :: Game
new_game = Game []

move :: Game -> Move -> Game
move (Game moves) m = Game (m:moves)
