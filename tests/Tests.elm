module Tests exposing (..)

import Blackjack exposing (..)
import ElmTest exposing (..)
import Json.Decode as JD exposing ((:=))
import Json.Encode as JE
import String

aceSpades = newCard Ace Spades
nineDiamonds = newCard Nine Diamonds
nineSpades = newCard Nine Spades
aceDiamonds = newCard Ace Diamonds
tenHearts = newCard Ten Hearts
queenClubs = newCard Queen Clubs

aS9D = newHand |> addCardToHand aceSpades |> addCardToHand nineDiamonds
aS9S = newHand |> addCardToHand aceSpades |> addCardToHand nineSpades
aSaD = newHand |> addCardToHand aceSpades |> addCardToHand aceDiamonds
aStH = newHand |> addCardToHand aceSpades |> addCardToHand tenHearts
tHqC = newHand |> addCardToHand tenHearts |> addCardToHand queenClubs
tDfCfHaS =
  newHand |> addCardToHand (newCard Two Diamonds) |> addCardToHand (newCard Four Clubs)
    |> addCardToHand (newCard Four Hearts) |> addCardToHand aceSpades
tHqCfH =
  newHand |> addCardToHand tenHearts |> addCardToHand queenClubs
    |> addCardToHand (newCard Four Hearts)
tHqCfHaD = tHqCfH |> addCardToHand aceDiamonds


splitTests : Test
splitTests =
  suite "isSplittable"
    [ test "Two different cards" (assert <| not <| isSplittable aS9D)
    , test "Different cards, same suit" (assert <| not <| isSplittable aS9S)
    , test "Same cards" (assert <| isSplittable aSaD)
    , test "Two face cards" (assert <| isSplittable tHqC)
    ]


hasAceTests : Test
hasAceTests =
  suite "hasAce"
    [ test "Two cards, one ace" (assert <| hasAce aS9D)
    , test "No ace" (assert <| not <| hasAce tHqC)
    , test "Two aces" (assert <| hasAce aSaD)
    , test "One ace, four cards" (assert <| hasAce tDfCfHaS)
    ]


isBlackjackTests : Test
isBlackjackTests =
  suite "isBlackjack"
    [ test "Two aces" (assert <| not <| isBlackjack aSaD)
    , test "No aces" (assert <| not <| isBlackjack tHqC)
    , test "Ace and face card" (assert <| isBlackjack aStH)
    , test "Ace and under card" (assert <| not <| isBlackjack aS9D)
    ]


bestScoreTests : Test
bestScoreTests =
  suite "bestScore"
    [ test "Ace and nine" (assertEqual (bestScore aS9D) 20)
    , test "Ace and ace" (assertEqual (bestScore aSaD) 12)
    , test "Two, four, four, and ace" (assertEqual (bestScore tDfCfHaS) 21)
    , test "Ten and queen" (assertEqual (bestScore tHqC) 20)
    , test "Bust w/o ace" (assertEqual (bestScore tHqCfH) 0)
    , test "Bust with ace" (assertEqual (bestScore tHqCfHaD) 0)
    ]


all : Test
all =
  suite "All tests"
    [ splitTests
    , hasAceTests
    , isBlackjackTests
    , bestScoreTests
    ]


main =
  runSuiteHtml all
