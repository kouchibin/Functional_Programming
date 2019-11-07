module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

-- A0
hand2 = Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)
sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty)
            , 1 + 1 + size Empty
            , 1 + 1 + 0
            , 2]


-- A1
rankToString :: Rank -> String
rankToString (Numeric n) = show n
rankToString r = show r

suitToString :: Suit -> String
suitToString s = show s

displayCard :: Card -> String
displayCard (Card r s) = rankToString r ++ " of " ++ suitToString s

display :: Hand -> String
display Empty     = ""
display (Add c h) = displayCard c ++ ", " ++ display h

-- A2

numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace _) h) = 1 + numberOfAces h
numberOfAces _ = 0

cardValue :: Card -> Integer
cardValue (Card (Numeric n) _) = n
cardValue (Card  Ace _)        = 11
cardValue _                    = 10

initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add c h) = cardValue c + initialValue h

value :: Hand -> Integer
value Empty = 0
value h     | initialValue h > 21 = initialValue h - ((numberOfAces h) * 10)
            | otherwise           = initialValue h

-- A3
-- Returns true if player loses
gameOver :: Hand -> Bool
gameOver h  = value h > 21 

-- A4
winner_points :: Integer -> Integer -> Player
winner_points g b | g <= 21 && b < g  = Guest
                  | b <= 21 && g <= b = Bank
                  | b > 21 && g <= 21 = Guest
                  | otherwise         = Bank

winner :: Hand -> Hand -> Player
winner gh bh = winner_points (value gh) (value bh)

-- B1
(<+) :: Hand -> Hand -> Hand
(<+) Empty h2     = h2
(<+) (Add c h) h2 = Add c (h <+ h2)  

-- Associativity Test
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

-- Size Test
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size (h1 <+ h2) == size h1 + size h2

-- B2 : Returns all cards in a list
allNumRank = [Numeric n | n <- [2..10]]
allRanks = allNumRank ++ [Jack, Queen, King, Ace]
allSuits = [Hearts, Spades, Diamonds, Clubs]
allCards = [Card r s | r <- allRanks, s <- allSuits]

builder :: [Card] -> Hand
builder []     = Empty
builder (x:xs) = Add x (builder xs) 

fullDeck :: Hand
fullDeck = builder allCards

-- B3
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add c d) h     = (d , (Add c h)) 
