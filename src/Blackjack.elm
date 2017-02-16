module Blackjack
    exposing
        ( Hand(..)
        , Card(..)
        , CardType(..)
        , CardSuit(..)
        , newHand
        , newCard
        , addCardToHand
        , addCardsToHand
        , suitOfCard
        , typeOfCard
        , isSplittable
        , isBust
        , hasAce
        , isSoft
        , isBlackjack
        , isTwentyOne
        , isHandBetterThan
        , isHandTiedWith
        , bestScore
        , cardValue
        , deserializeCard
        , serializeType
        , serializeSuit
        , deserializeType
        , deserializeSuit
        )

{-| This library provides a few utility functions
for a Blackjack application. It can compare hands
and calculate the best score for a given set of cards.

# Types
@docs Hand, Card, CardType, CardSuit

# Construction
@docs newHand, newCard, addCardToHand, addCardsToHand, suitOfCard, typeOfCard

# Utility Functions
@docs isSplittable, isBust, hasAce, isSoft, isBlackjack, isTwentyOne, isHandBetterThan, isHandTiedWith, bestScore. cardValue

# Serialization
@docs deserializeSuit, deserializeType, deserializeCard, serializeSuit, serializeType

-}


{-| Represents a hand in Blackjack. Can be
an arbitrary number of cards.
-}
type Hand
    = BjHand (List Card)


{-| Represents a standard playing card.
-}
type Card
    = BjCard { type_ : CardType, suit : CardSuit }


{-| -}
type CardType
    = Ace
    | King
    | Queen
    | Jack
    | Ten
    | Nine
    | Eight
    | Seven
    | Six
    | Five
    | Four
    | Three
    | Two


{-| -}
type CardSuit
    = Clubs
    | Diamonds
    | Hearts
    | Spades


{-| Construct an empty hand. Can be
either a player's or dealer's hand.
-}
newHand : Hand
newHand =
    BjHand []


{-| Construct a card given the type and suit.
-}
newCard : CardType -> CardSuit -> Card
newCard type_ suit =
    BjCard { type_ = type_, suit = suit }


{-| Add a card to an existing hand. Order of
cards is irrelavent.
-}
addCardToHand : Card -> Hand -> Hand
addCardToHand card (BjHand hand) =
    BjHand (card :: hand)


{-| Add cards to an existing hand. Order of
cards is irrelavent.
-}
addCardsToHand : List Card -> Hand -> Hand
addCardsToHand cards (BjHand hand) =
    BjHand (cards ++ hand)


{-| Returns the suit of a given card.
-}
suitOfCard : Card -> CardSuit
suitOfCard (BjCard c) =
    c.suit


{-| Returns the type of a given card.
-}
typeOfCard : Card -> CardType
typeOfCard (BjCard c) =
    c.type_


{-| Test if a hand is splittable. A hand
is only splittable if it has two cards,
both of the same value.

    hand = newHand
      |> addCardToHand (newCard Ace Diamonds)
      |> addCardToHand (newCard Ace Clubs)

    isSplittable hand == True
-}
isSplittable : Hand -> Bool
isSplittable (BjHand hand) =
    case hand of
        [ (BjCard c1) as card1, (BjCard c2) as card2 ] ->
            c1.type_ == c2.type_ || (isVirtualTen card1 && isVirtualTen card2)

        _ ->
            False


{-| Test if a hand is busted. A hand
is busted if all possible values are
greater than 21.

    hand1 = newHand
      |> addCardToHand (newCard Ace Diamonds)
      |> addCardToHand (newCard Ace Clubs)
    hand2 = newHand
      |> addCardToHand (newCard Ten Diamonds)
      |> addCardToHand (newCard Queen Clubs)
      |> addCardToHand (newCard Two Clubs)

    isBust hand1 == False
    isBust hand2 == True
-}
isBust : Hand -> Bool
isBust hand =
    (bestScore hand) == 0


{-| Test if one of the cards in the hand
is an Ace. Useful if you want to prompt
the user to double-down.

    hand = newHand
      |> addCardToHand (newCard Ace Diamonds)
      |> addCardToHand (newCard Ace Clubs)

    hasAce hand == True
-}
hasAce : Hand -> Bool
hasAce (BjHand hand) =
    List.any (\(BjCard card) -> card.type_ == Ace) hand


{-| Test if the hand is "soft". Indicates hand can
receive an extra card without the threat of busting.

    hand1 = newHand
      |> addCardToHand (newCard Ace Diamonds)
      |> addCardToHand (newCard Six Clubs)

    isSoft hand1 == True
-}
isSoft : Hand -> Bool
isSoft hand =
    let
        scores =
            potentialScores hand

        ace =
            hasAce hand
    in
        ace
            && List.any (\s -> s <= 11) scores


{-| Test if a hand is a Blackjack.
A hand is Blackjack if it is composed of
one Ace and one face card or 10.

    hand1 = newHand
      |> addCardToHand (newCard Ace Diamonds)
      |> addCardToHand (newCard Ten Clubs)
    hand2 = newHand
      |> addCardToHand (newCard Ace Diamonds)
      |> addCardToHand (newCard Ace Clubs)

    isBlackjack hand1 == True
    isBlackjack hand2 == False
-}
isBlackjack : Hand -> Bool
isBlackjack (BjHand hand) =
    case hand of
        [ (BjCard c1) as card1, (BjCard c2) as card2 ] ->
            case ( c1.type_, c2.type_ ) of
                ( Ace, _ ) ->
                    isVirtualTen card2

                ( _, Ace ) ->
                    isVirtualTen card1

                _ ->
                    False

        _ ->
            False


{-| Test if a hand's value is 21. This
will return True for Blackjack hands as
well as non-Blackjack hands.

    hand1 = newHand
      |> addCardToHand (newCard Ace Diamonds)
      |> addCardToHand (newCard Ten Clubs)
    hand2 = newHand
      |> addCardToHand (newCard Ace Diamonds)
      |> addCardToHand (newCard Ace Clubs)
    hand3 = newHand
      |> addCardToHand (newCard Ace Diamonds)
      |> addCardToHand (newCard Ace Clubs)
      |> addCardToHand (newCard Nine Clubs)

    isTwentyOne hand1 == True
    isTwentyOne hand2 == False
    isTwentyOne hand3 == True
-}
isTwentyOne : Hand -> Bool
isTwentyOne hand =
    (bestScore hand) == 21


{-| Test if one hand has a better score than
another hand. Useful to compare a player's
hand against a dealer's. The hand with the
highest score while less than or equal to 21
is the best hand.

    hand1 = newHand
      |> addCardToHand (newCard Seven Diamonds)
      |> addCardToHand (newCard Ten Clubs)
    hand2 = newHand
      |> addCardToHand (newCard Five Diamonds)
      |> addCardToHand (newCard Ten Clubs)

    isHandBetterThan hand1 hand2 == True
-}
isHandBetterThan : Hand -> Hand -> Bool
isHandBetterThan hand1 hand2 =
    (bestScore hand1) > (bestScore hand2)


{-| Test if one hand has the same score as
another hand. Useful to compare a player's
hand against a dealer's.

    hand1 = newHand
      |> addCardToHand (newCard Seven Diamonds)
      |> addCardToHand (newCard Ten Clubs)
    hand2 = newHand
      |> addCardToHand (newCard Five Diamonds)
      |> addCardToHand (newCard Ten Clubs)
      |> addCardToHand (newCard Two Hearts)

    isHandTiedWith hand1 hand2 == True
-}
isHandTiedWith : Hand -> Hand -> Bool
isHandTiedWith hand1 hand2 =
    (bestScore hand1) == (bestScore hand2)


{-| Returns the highest score a hand can have.
If all possible scores are greater than 21, 0
is returned.

    hand1 = newHand
      |> addCardToHand (newCard Seven Diamonds)
      |> addCardToHand (newCard Ace Clubs)
    hand2 = newHand
      |> addCardToHand (newCard Five Diamonds)
      |> addCardToHand (newCard Ten Clubs)
      |> addCardToHand (newCard Nine Hearts)

    bestScore hand1 == 18
    bestScore hand2 == 0
-}
bestScore : Hand -> Int
bestScore hand =
    let
        goodScores =
            potentialScores hand |> List.filter (\c -> c <= 21)
    in
        Maybe.withDefault 0 (List.head goodScores)


{-| Get the number value of the card.
Returns `0` for aces.
-}
cardValue : Card -> Int
cardValue (BjCard card) =
    case card.type_ of
        King ->
            10

        Queen ->
            10

        Jack ->
            10

        Ten ->
            10

        Nine ->
            9

        Eight ->
            8

        Seven ->
            7

        Six ->
            6

        Five ->
            5

        Four ->
            4

        Three ->
            3

        Two ->
            2

        _ ->
            0



-- Private Functions


potentialScores : Hand -> List Int
potentialScores (BjHand hand) =
    let
        ( aces, noAces ) =
            List.partition (\(BjCard c) -> c.type_ == Ace) hand

        preAcesSum =
            List.sum <| List.map cardValue noAces

        func (BjCard card) scores =
            let
                plus1 =
                    List.map (\s -> s + 1) scores

                plus11 =
                    List.map (\s -> s + 11) scores
            in
                plus11 ++ plus1

        afterAces =
            List.foldl func [ preAcesSum ] aces
    in
        List.sortWith (\a b -> compare b a) afterAces


isVirtualTen : Card -> Bool
isVirtualTen (BjCard card) =
    case card.type_ of
        King ->
            True

        Queen ->
            True

        Ten ->
            True

        Jack ->
            True

        _ ->
            False


{-| Convert to Ints to a Card
-}
deserializeCard : Int -> Int -> Card
deserializeCard type_ suit =
    BjCard
        { type_ = deserializeType type_
        , suit = deserializeSuit suit
        }


{-| Convert an Int to a CardSuit
-}
deserializeSuit : Int -> CardSuit
deserializeSuit suit =
    case suit of
        0 ->
            Clubs

        1 ->
            Diamonds

        2 ->
            Hearts

        _ ->
            Spades


{-| Convert an Int to a CardType
-}
deserializeType : Int -> CardType
deserializeType type_ =
    case type_ of
        0 ->
            Two

        1 ->
            Three

        2 ->
            Four

        3 ->
            Five

        4 ->
            Six

        5 ->
            Seven

        6 ->
            Eight

        7 ->
            Nine

        8 ->
            Ten

        9 ->
            Jack

        10 ->
            Queen

        11 ->
            King

        _ ->
            Ace


{-| Convert CardSuit to an Int
-}
serializeSuit : CardSuit -> Int
serializeSuit suit =
    case suit of
        Clubs ->
            0

        Diamonds ->
            1

        Hearts ->
            2

        Spades ->
            3


{-| Convert CardType to an Int
-}
serializeType : CardType -> Int
serializeType type_ =
    case type_ of
        Two ->
            0

        Three ->
            1

        Four ->
            2

        Five ->
            3

        Six ->
            4

        Seven ->
            5

        Eight ->
            6

        Nine ->
            7

        Ten ->
            8

        Jack ->
            9

        Queen ->
            10

        King ->
            11

        Ace ->
            12
