package http

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.syntax.all._
import effects.SharedStateHomework.Cache
import http.Protocol._
import org.http4s._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed, as well as the maximum number of attempts.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// Use HTTP or WebSocket for communication. The exact protocol and message format to use is not specified and
// should be designed while working on the task.

object Protocol {
  final case class Game(number: Int, attempts: Int)
}

object GuessServer extends IOApp {
  //curl "localhost:9001/gamestart" -b "minimum=0;maximum=0;attempts=5"
  private def start(cache: Cache[IO, Int, Game]) = HttpRoutes.of[IO] {
    case req @ GET -> Root / "gamestart" =>
      val min =
        req.cookies.find(_.name == "minimum").flatMap(_.content.toIntOption)
      val max =
        req.cookies.find(_.name == "maximum").flatMap(_.content.toIntOption)
      val attempts =
        req.cookies.find(_.name == "attempts").flatMap(_.content.toIntOption)
      generateRandomValue(min, max) match {
        case Some(value) =>
          attempts match {
            case Some(attemptsNumber) => {
              val gameId = generateGameId(cache)
              val game = Game(value, attemptsNumber)
              cache.put(gameId, game) *> Ok(s"Game #${gameId} has started")
                .map(_.addCookie("gameId", gameId.toString))
            }

            case None => BadRequest("You did not provide number of attempts")
          }
        case None => BadRequest("You did not provides scope of your number")
      }
  }

  //curl "localhost:9001/gamestep" -b "gameId=?;userNumber=2"
  private def gameStep(cache: Cache[IO, Int, Game]) = HttpRoutes.of[IO] {
    case req @ GET -> Root / "gamestep" =>
      val gameId =
        req.cookies.find(_.name == "gameId").flatMap(_.content.toIntOption)
      val userNumber =
        req.cookies.find(_.name == "userNumber").flatMap(_.content.toIntOption)
      gameId match {
        case Some(value) =>
          userNumber match {
            case Some(uNumber) =>
              cache
                .get(value)
                .map {
                  case Some(game) => getResponse(cache, game, uNumber, value)
                  case None       => BadRequest("Game does not exist")
                }
                .flatten
            case None => BadRequest("You did not provided your number")
          }
        case None => BadRequest("You did not provided game ID")
      }
  }

  def getResponse(cache: Cache[IO, Int, Game],
                  game: Game,
                  userNumber: Int,
                  id: Int) = {
    if (game.number == userNumber)
      cache.remove(id) *> Ok("You won").map(
        _.addCookie("gameStatus", "false".toString)
      )
    else if (game.attempts > 1)
      if (game.number > userNumber)
        cache.update(id, Game(game.number, game.attempts - 1)) *> Ok(
          "Your number is greater than guessed"
        ).map(_.addCookie("gameStatus", "true".toString))
      else
        cache.update(id, Game(game.number, game.attempts - 1)) *> Ok(
          "Your number is lower than guessed"
        ).map(_.addCookie("gameStatus", "true".toString))
    else
      cache.remove(id) *> Ok(
        "Your answer is wrong and that was your last attempt"
      ).map(_.addCookie("gameStatus", "false".toString))
  }

  def generateRandomValue(min: Option[Int], max: Option[Int]): Option[Int] = {
    val r = scala.util.Random
    min match {
      case Some(minimum) =>
        max match {
          case Some(maximum) =>
            Some(minimum + r.nextInt((maximum - minimum) + 1))
          case None => None
        }
      case None => None
    }
  }

  def generateGameId(cache: Cache[IO, Int, Game]): Int = {
    val r = scala.util.Random
    r.nextInt(Int.MaxValue)
  }

  private[http] def httpApp(cache: Cache[IO, Int, Game]) = {
    start(cache) <+> gameStep(cache)
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    for {
      cache <- Cache.of[IO, Int, Game](3600.seconds, 100.seconds)
      _ <- BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = 9001, host = "localhost")
        .withHttpApp(httpApp(cache))
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    } yield ExitCode.Success

}
object GuessClient extends IOApp {
  private val uri = uri"http://localhost:9001"
  private val rand = scala.util.Random
  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  private def getMin(): IO[Int] = IO(rand.nextInt(10))
  private def getMax(min: Int): IO[Int] = IO(min + rand.nextInt((20 - min) + 1))
  private def getAttempt(): IO[Int] = IO(1 + rand.nextInt(9))

  def startGame(client: Client[IO],
                min: Int,
                max: Int,
                attempts: Int): IO[Int] = {
    val startRequest = Method
      .GET(uri / "gamestart")
      .map(
        request =>
          request
            .addCookie("minimum", min.toString)
            .addCookie("maximum", max.toString)
            .addCookie("attempts", attempts.toString)
      )
    val id = for {
      _ <- printLine(
        s"Range for this game will be from ${min} to ${max}, number of attempts - ${attempts}"
      )
      request <- startRequest
      response <- client.run(request).use(resp => IO(resp))
      gameId <- IO(response.cookies.find(_.name == "gameId"))
    } yield gameId
    id.map {
      case Some(value) =>
        value.content.toIntOption match {
          case Some(gameId) => gameId
          case None         => -1
        }
      case None => -1
    }
  }

  def playGame(client: Client[IO], id: Int, min: Int, max: Int): IO[Unit] = {
    if (id < 0) printLine("Something went wrong")
    else {

      val myGuess = min + rand.nextInt((max - min) + 1)
      val curRequest = Method
        .GET(uri / "gamestep")
        .map(
          request =>
            request
              .addCookie("gameId", id.toString)
              .addCookie("userNumber", myGuess.toString)
        )
      for {
        _ <- printLine(s"Guessing number - ${myGuess}")
        request <- curRequest
        response <- client.run(request).use(resp => IO(resp))
        _ <- response.as[String] >>= printLine
        cookie <- IO(response.cookies.find(_.name == "gameStatus"))
        _ <- cookie match {
          case Some(value) =>
            if (value.content.toBooleanOption.getOrElse(false))
              playGame(client, id, min, max)
            else printLine("Game over")
          case None => printLine("Game status not found")
        }
      } yield ()
    }
  }

  def run(args: List[String]): IO[ExitCode] = {
    BlazeClientBuilder[IO](ExecutionContext.global).resource
      .parZip(Blocker[IO])
      .use {
        case (client, blocker) =>
          for {
            _ <- printLine("Starting game...")
            min <- getMin()
            max <- getMax(min)
            attempt <- getAttempt()
            gameID <- startGame(client, min, max, attempt)
            _ <- playGame(client, gameID, min, max)
          } yield ()
      }
      .as(ExitCode.Success)
  }
}
