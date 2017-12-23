{-
Minh Vo & Vuong Khuat
CS330 - Functional Programming
Implementation of Cards
mbvo14_vdkhuat16_cards.hs
-}

module Cards where
import System.Random
import Data.List

{-
Data type for suits.
-}
data Suit = Clubs | Diamonds | Hearts | Spades
    deriving (Show, Eq, Ord)

{-
Data type for ranks.
-}
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord)

{-
Type synonym for a playing card, which is a tuple of (Rank, Suit).
-}
type PlayingCard = (Rank, Suit)

{-
fullDeck is a full list of 52 cards, unshuffled.
-}
fullDeck :: [PlayingCard]
fullDeck = [(rank, suit) | rank <- [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace],
                            suit <- [Clubs, Diamonds, Hearts, Spades]]

{-
A representation of a full deck of cards that is shuffled.
-}
shuffledDeck :: IO [PlayingCard]
shuffledDeck = do
    gen <- getStdGen
    let randoms = (randomRs (0, 51) gen) :: [Int]
    -- Generate a random list from 0 to 51
    let randomizedPositions = take 52 (nub randoms)
    newStdGen -- this is to avoid using the same generator every time.
    return [fullDeck !! x | x <- randomizedPositions]

{-
cardToString turns a playing card into a string "[Rank] of [Suit]".
This is for the purpose of displaying the card.
-}
cardToString :: PlayingCard -> String
cardToString (rank, suit) = show rank ++ " of " ++ show suit
