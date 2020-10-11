package adt

import basics.ThirdHomework.ErrorMessage

object FifthHomework {
  // Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
  // task you completed to join the bootcamp. Use your best judgement about particular data types to include
  // in the solution, you can model concepts like:
  //
  // 1. Suit
  // 2. Rank
  // 3. Card
  // 4. Hand (Texas or Omaha)
  // 5. Board
  // 6. Poker Combination (High Card, Pair, etc.)
  // 7. Test Case (Board & Hands to rank) //income
  // 8. Test Result (Hands ranked in a particular order) //outcome
  //
  // Make sure the defined model protects against invalid data. Use value classes and smart constructors as
  // appropriate. Place the solution under `adt` package in your homework repository.

  final case class Suit(value: String) extends AnyVal
  object Suit {
    val possibleSuits = List("h", "d", "c", "s")
    def create(value: String): Either[ErrorMessage, Suit] = {
      if (possibleSuits.contains(value))
        Right(Suit(value))
      else
        Left(ErrorMessage("Incorrect suit format"))
    }
  }

  final case class Rank(value: String) extends AnyVal
  object Rank {
    val possibleRanks = List("2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A")
    def create(value: String): Either[ErrorMessage, Rank] = {
      if (possibleRanks.contains(value))
        Right(Rank(value))
      else
        Left(ErrorMessage("Incorrect rank format"))
    }
  }

  final case class Card(suit: Suit, rank: Rank)

  final case class Hand(cards: List[Card]) extends AnyVal
  object Hand {
    def create(cards: List[Card]): Either[ErrorMessage, Hand] = {
      if(cards.length == 2 || cards.length == 4)
        Right(Hand(cards))
      else
        Left(ErrorMessage("Incorrect number of a cards"))
    }
  }

  final case class Board(cards: List[Card]) extends AnyVal
  object Board {
    def create(cards: List[Card]): Either[ErrorMessage, Board] = {
      if(cards.length == 5)
        Right(Board(cards))
      else
        Left(ErrorMessage("Incorrect number of a cards"))
    }
  }

  sealed trait PokerCombinations
  object PokerCombinations {
    final case class straightFlush(board:Board, hand:Hand)
    final case class fourOfAKind(board:Board, hand:Hand)
    final case class fullHouse(board:Board, hand:Hand)
    final case class flush(board:Board, hand:Hand)
    final case class straight(board:Board, hand:Hand)
    final case class threeOfAKind(board:Board, hand:Hand)
    final case class twoPairs(board:Board, hand:Hand)
    final case class pair(board:Board, hand:Hand)
    final case class highCard(board:Board, hand:Hand)
  }

  final case class Input(input: String) extends AnyVal
  object Input {
    def create(input: String): Either[ErrorMessage, Input] = {
      val convertedInput = input.trim.replaceAll(" +", " ")
      val inputValues = convertedInput.split(" ").toList
      inputValues match {
        case _ if input.isEmpty => Left(ErrorMessage("Empty value"))
        case _ if (inputValues.length < 3) => Left(ErrorMessage("Not enough values"))
        case _ if (inputValues.head.length != 10) => Left(ErrorMessage("Incorrect board size"))
        case _ if (inputValues(1).length != 4 && inputValues(1).length != 8) => Left(ErrorMessage("Incorrect hand size"))
        case _ :: xs => xs match {
          case x if !x.forall(v => v.length == inputValues(1).length) => Left(ErrorMessage("Hand values sizes are different"))
          case _ => Right(Input(convertedInput))
        }
      }
    }
  }

  final case class Output(board: Board, hands: List[Hand])
}
