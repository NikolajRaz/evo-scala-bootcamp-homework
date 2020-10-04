package basics

import scala.io.Source

object ThirdHomework {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  sealed trait Result
  final case class CorrectResult(value: String) extends Result
  // adjust Result as required to match requirements

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    import Command._

    try {
      x.split(" ").toList match {
        case _ :: Nil => Left(ErrorMessage("Empty string or values are missing"))
        case x :: xs => x match {
          case _ if xs.isEmpty => Left(ErrorMessage("Values are missing"))
          case "divide" => if (xs.length == 2 && xs(1).toDouble != 0) Right(Divide(xs.head.toDouble, xs(1).toDouble)) else Left(ErrorMessage("Incorrect format of dividend and divisor"))
          case "sum" => Right(Sum(xs.map(x => x.toDouble)))
          case "average" => Right(Average(xs.map(x => x.toDouble)))
          case "min" => Right(Min(xs.map(x => x.toDouble)))
          case "max" => Right(Max(xs.map(x => x.toDouble)))
          case _ => Left(ErrorMessage("Wrong command was used"))
        }
      }
    }
    catch {
      case ex: Exception => Left(ErrorMessage("Wrong command format"))
    }
    // implement this method
    // Implementation hints:
    // You can use String#split, convert to List using .toList, then pattern match on:
    //   case x :: xs => ???

    // Consider how to handle extra whitespace gracefully (without errors).
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    import Command._

    x match {
      case x:Divide => Right(CorrectResult(s"${x.dividend} divided by ${x.divisor} is ${x.dividend/x.divisor}"))
      case x:Sum => Right(CorrectResult(s"the sum of ${x.numbers.mkString(" ")} is ${x.numbers.sum}"))
      case x:Average => Right(CorrectResult(s"the average of ${x.numbers.mkString(" ")} is ${x.numbers.sum/x.numbers.length}"))
      case x:Min => Right(CorrectResult(s"the minimum of ${x.numbers.mkString(" ")} is ${x.numbers.min}"))
      case x:Max => Right(CorrectResult(s"the maximum of ${x.numbers.mkString(" ")} is ${x.numbers.max}"))
      case _ => Left(ErrorMessage("Error in calculations"))
    }
  }

  def renderResult(x: Result): String = x match{
    case x:CorrectResult => x.value
  }

  def process(x: String): String = {
    (for{
      command <- parseCommand(x)
      calculate <- calculate(command)
    }yield(calculate)).fold(left => s"Error: ${left.value}", right => renderResult(right))

    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.
    // implement using a for-comprehension
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}