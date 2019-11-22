module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random

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
display (Add card hand) = displayCard card ++ "\n" ++ display hand

--------------------------------------------------------------------
-- A2 | Calculate the value of a hand.

-- | Returns the number of Aces in a hand.
numberOfAces :: Hand -> Integer
numberOfAces Empty                = 0
numberOfAces (Add (Card Ace _) hand) = 1 + numberOfAces hand
numberOfAces (Add _ hand)            = numberOfAces hand

-- | Returns the value of a card. Ace = 11. 
cardValue :: Card -> Integer
cardValue (Card (Numeric n) _) = n
cardValue (Card Ace _)         = 11
cardValue _                    = 10

-- | Returns the initial value (Ace = 11) of a hand.
initialValue :: Hand -> Integer
initialValue Empty     = 0
initialValue (Add card hand) = cardValue card + initialValue hand

-- | Returns the value of a hand.
value :: Hand -> Integer
value Empty = 0
value hand | initialValue hand > 21 = initialValue hand - ((numberOfAces hand) * 10)
        | otherwise           = initialValue hand


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
gameOver hand = value hand > 21 

--------------------------------------------------------------------
-- A4 | Decide which player wins.

-- | Given players' points and returns winner.
winner_points :: Integer -> Integer -> Player
winner_points guest bank | guest <= 21 && bank < guest  = Guest
                  | bank <= 21 && guest <= bank = Bank
                  | bank > 21 && guest <= 21 = Guest
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
(<+) (Add card hand) h2 = Add card (hand <+ h2)  

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
draw (Add card deck) hand = (deck , (Add card hand)) 

--------------------------------------------------------------------
-- B4 | Bank player logic.

playBankHelper :: Hand -> Hand -> Hand
playBankHelper deck hand | value hand < 16 = playBankHelper smallerDeck biggerHand
                         | otherwise       = hand
    where (smallerDeck, biggerHand) = draw deck hand

-- Returns a hand for the bank player from a deck.
playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty

--------------------------------------------------------------------
-- B5 | Shuffle deck.

-- Removes the nth card from the deck, returns the card and the remaining deck.
remove :: Int -> Hand -> (Card, Hand) 
remove n Empty                                      = error "Hand is empty."
remove n (Add card hand) | n <= 0                   = error "n is smaller or equal to 0"
                         | n > size (Add card hand) = error "n is greater than the size of the hand"
                         | n == 1                   = (card, hand) 
                         | otherwise                = (drawnCard, Add card smallerHand)
                               where (drawnCard, smallerHand) = remove (n-1) hand

-- Helper function for shuffling deck. 
shuffleHelper :: StdGen -> Hand -> Hand -> (Hand, Hand, StdGen)
shuffleHelper g deck shuffledHand | size deck == 0    = (deck, shuffledHand, g) 
                                  | otherwise         = shuffleHelper g1 smallerDeck newShuffled
                                      where (random, g1)          = randomR (1, size deck ) g 
                                            (rCard, smallerDeck)  = remove random deck
                                            newShuffled           = (Add rCard shuffledHand)

-- Given a random generator and and deck, returns the shuffled deck.
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck gen deck = shuffled
    where (_, shuffled , _) = shuffleHelper gen deck Empty

-- Test if the shuffled deck has the same cards.
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g card hand =
    card `belongsTo` hand == card `belongsTo` shuffleDeck g hand

-- See if a card belongs to a hand.
belongsTo :: Card -> Hand -> Bool
card `belongsTo` Empty = False
card `belongsTo` (Add card' hand) = card == card' || card `belongsTo` hand

-- Test if the shuffled deck has the same size as the original one.
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle gen deck = size deck == size (shuffleDeck gen deck)  

--------------------------------------------------------------------
-- B6 | Interface

implementation = Interface
  { iFullDeck = fullDeck
  , iValue    = value
  , iDisplay  = display
  , iGameOver = gameOver
  , iWinner   = winner
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffleDeck
  }

main :: IO ()
main = runGame implementation
                            
