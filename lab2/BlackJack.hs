module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

--------------------------------------------------------------------
-- A0 | The length of hand2 should be 2.

hand2 = Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)
sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty)
            , 1 + 1 + size Empty
            , 1 + 1 + 0
            , 2]

--------------------------------------------------------------------
-- A1 | Pretty print a hand.

rankToString :: Rank -> String
rankToString (Numeric n) = show n
rankToString r = show r

suitToString :: Suit -> String
suitToString s = show s

-- | Pretty print a single card.
displayCard :: Card -> String
displayCard (Card r s) = rankToString r ++ " of " ++ suitToString s

-- | Shows the cards in a hand in a nice format.
display :: Hand -> String
display Empty     = ""
display (Add c h) = displayCard c ++ "\n" ++ display h

--------------------------------------------------------------------
-- A2 | Calculate the value of a hand.

-- | Returns the number of Aces in a hand.
numberOfAces :: Hand -> Integer
numberOfAces Empty                = 0
numberOfAces (Add (Card Ace _) h) = 1 + numberOfAces h
numberOfAces (Add _ h)            = numberOfAces h

-- | Returns the value of a card. Ace = 11. 
cardValue :: Card -> Integer
cardValue (Card (Numeric n) _) = n
cardValue (Card Ace _)         = 11
cardValue _                    = 10

-- | Returns the initial value (Ace = 11) of a hand.
initialValue :: Hand -> Integer
initialValue Empty     = 0
initialValue (Add c h) = cardValue c + initialValue h

-- | Returns the value of a hand.
value :: Hand -> Integer
value Empty = 0
value h | initialValue h > 21 = initialValue h - ((numberOfAces h) * 10)
        | otherwise           = initialValue h


-- | Some tests.
card_A  = Card Ace Hearts
card_3  = Card (Numeric 3) Hearts
card_10 = Card King Spades

-- Without Ace.
hand_0  = Empty
hand_13 = Add card_3 (Add card_10 Empty)
hand_23 = Add card_10 hand_13

-- With Ace.
hand_11 = Add card_A Empty
hand_2  = Add card_A hand_11
hand_21 = Add card_A (Add card_10 Empty)
hand_14 = Add card_3 hand_21

test_value = [0, 13, 23, 11, 2, 21, 14] == 
             map value [hand_0, hand_13, hand_23, hand_11, hand_2, hand_21, hand_14]

--------------------------------------------------------------------
-- A3 | Test if player got busted.

gameOver :: Hand -> Bool
gameOver h = value h > 21 

--------------------------------------------------------------------
-- A4 | Decide which player wins.

-- | Given players' points and returns winner.
winner_points :: Integer -> Integer -> Player
winner_points g b | g <= 21 && b < g  = Guest
                  | b <= 21 && g <= b = Bank
                  | b > 21 && g <= 21 = Guest
                  | otherwise         = Bank

-- | Given players' hands and returns the winner.
winner :: Hand -> Hand -> Player
winner gh bh = winner_points (value gh) (value bh)

-- | Some tests.

-- Both not busted.
bank_1 = winner_points 6 10
bank_2 = winner_points 10 10
bank_3 = winner_points 10 21
bank_4 = winner_points 21 21
guest_1 = winner_points 11 10
guest_2 = winner_points 21 10

-- Guest got busted.
bank_5 = winner_points 22 1
bank_6 = winner_points 22 21

-- Bank got busted.
guest_3 = winner_points 1 22
guest_4 = winner_points 21 22

-- Both busted.
bank_7 = winner_points 22 22

test_winner = [bank_1, bank_2, bank_3, bank_4, bank_5, bank_6, bank_7] == replicate 7 Bank &&
              [guest_1, guest_2, guest_3, guest_4] == replicate 4 Guest

--------------------------------------------------------------------
-- B1 | Put one hand on top of another.

(<+) :: Hand -> Hand -> Hand
(<+) Empty h2     = h2
(<+) (Add c h) h2 = Add c (h <+ h2)  

-- | Associativity property test.
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

-- | Size Test
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size (h1 <+ h2) == size h1 + size h2

--------------------------------------------------------------------
-- B2 | Returns a full deck of cards.

allNumRank = [Numeric n | n <- [2..10]]
allRanks = allNumRank ++ [Jack, Queen, King, Ace]
allSuits = [Hearts, Spades, Diamonds, Clubs]
allCards = [Card r s | r <- allRanks, s <- allSuits]

-- | Converts a list of cards into a Hand.
handBuilder :: [Card] -> Hand
handBuilder []     = Empty
handBuilder (x:xs) = Add x (handBuilder xs) 

-- | Returns a full deck of cards.
fullDeck :: Hand
fullDeck = handBuilder allCards

--------------------------------------------------------------------
-- B3 | Draw a card from one hand to another.

draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add c d) h = (d , (Add c h)) 
