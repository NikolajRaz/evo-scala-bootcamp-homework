package effects

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

/*
 * Homework 1. Provide your own implementation of a subset of `IO` functionality.
 *
 * Provide also tests for this functionality in EffectsHomework1Spec (which you should create).
 *
 * Refer to:
 *  - https://typelevel.org/cats-effect/datatypes/io.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO$.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO.html
 * about the meaning of each method as needed.
 *
 * There are two main ways how to implement IO:
 * - Executable encoding  - express every constructor and operator for our model in terms of its execution
 * - Declarative encoding - express every constructor and operator for our model as pure data in a recursive
 *                          tree structure
 *
 * While the real Cats Effect IO implementation uses declarative encoding, it will be easier to solve this
 * task using executable encoding, that is:
 *  - Add a `private val run: () => A` parameter to the class `IO` private constructor
 *  - Have most of the methods return a `new IO(...)`
 *
 * Ask questions in the bootcamp chat if stuck on this task.
 */

object EffectsHomework1 {
  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global

  final class IO[A](private val run: () => A) {
    def map[B](f: A => B): IO[B] = IO(f(run()))

    def flatMap[B](f: A => IO[B]): IO[B] = f(run())

    def *>[B](another: IO[B]): IO[B] = flatMap(_ => another)

    def as[B](newValue: => B): IO[B] = map(_ => newValue)

    def void: IO[Unit] = map(_ => ())

    def attempt: IO[Either[Throwable, A]] = IO(Try(run()).toEither)

    def option: IO[Option[A]] = IO(Try(run()).toOption)

    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = attempt.map {
      case Left(exception) => f(exception).run()
      case Right(value)    => value
    }

    def redeem[B](recover: Throwable => B, map: A => B): IO[B] = attempt.map {
      case Left(exception) => recover(exception)
      case Right(value)    => map(value)
    }

    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] =
      attempt.map {
        case Left(exception) => recover(exception).run()
        case Right(value)    => bind(value).run()
      }

    def unsafeRunSync(): A = run()

    def unsafeToFuture(): Future[A] = Future(run())
  }

  object IO {
    def apply[A](body: => A): IO[A] = new IO(() => body)

    def suspend[A](thunk: => IO[A]): IO[A] = thunk

    def delay[A](body: => A): IO[A] = apply(body)

    def pure[A](a: A): IO[A] = IO(a)

    def fromEither[A](e: Either[Throwable, A]): IO[A] = e match {
      case Left(value)  => raiseError(value)
      case Right(value) => IO(value)
    }

    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] =
      option match {
        case None        => raiseError(orElse)
        case Some(value) => IO(value)
      }

    def fromTry[A](t: Try[A]): IO[A] = t match {
      case Failure(exception) => raiseError(exception)
      case Success(value)     => IO(value)
    }

    def none[A]: IO[Option[A]] = IO(None)

    def raiseError[A](e: Throwable): IO[A] = IO(throw e)

    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] =
      if (cond) unit else raiseError(e)

    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] =
      if (cond) raiseError(e) else unit

    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] =
      if (cond) unit else action

    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] =
      if (cond) action else unit

    val unit: IO[Unit] = IO(())
  }

}
