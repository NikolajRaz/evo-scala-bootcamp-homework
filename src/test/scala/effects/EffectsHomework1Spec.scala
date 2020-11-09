package effects

import effects.EffectsHomework1._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.duration._
import scala.concurrent.Await
import scala.util.Try

class EffectsHomework1Spec extends AnyWordSpec {
  val example1 = IO[Int](2)
  val example2 = IO[Int](3)
  val example3 = IO[Int](4 / 0)

  "unsafeRunSync" should {
    "return body value" in {
      example1.unsafeRunSync() shouldEqual 2
    }
  }

  "map" should {
    "return IO which contains 8" in {
      example1.map(x => x * 4).unsafeRunSync() shouldEqual 8
    }
    "return IO which contains string - 2" in {
      example1.map(_.toString).unsafeRunSync() shouldEqual "2"
    }
  }

  "flatMap" should {
    "make sequence of actions" in {
      example1
        .flatMap(x => example1.map(y => y * x))
        .unsafeRunSync() shouldEqual 4
    }
  }

  "*>" should {
    "runs the current IO, then runs the parameter, keeps its result" in {
      example1 *> example2 shouldEqual example2
    }
  }

  "as" should {
    "replaces the result of this IO with the given value" in {
      example1.as(4).unsafeRunSync() shouldEqual 4
    }
  }

  "void" should {
    "return an IO[Unit]" in {
      example1.void.unsafeRunSync() shouldEqual ()
    }
  }

  "attempt" should {
    "return IO with Right" in {
      example1.attempt.unsafeRunSync() shouldEqual Right(2)

    }

    "return IO with left" in {
      //I had converted values to string to compare exceptions
      example3.attempt.unsafeRunSync().toString shouldEqual Left(
        new ArithmeticException("/ by zero")
      ).toString
    }
  }

  "option" should {
    "return IO with Some(2)" in {
      example1.option.unsafeRunSync() shouldEqual Some(2)
    }
    "return none" in {
      example3.option.unsafeRunSync() shouldEqual None
    }
  }

  "handleErrorWith" should {
    "return IO which contains 2 instead of error" in {
      example3
        .handleErrorWith(_ => example1)
        .unsafeRunSync() shouldEqual 2
    }
  }

  "redeem" should {
    "return IO which contains 8" in {
      example1.redeem(_ => "Error", x => x * 4).unsafeRunSync() shouldEqual 8

    }
    "return IO which contains string - Error" in {
      example3
        .redeem(_ => "Error", x => x * 4)
        .unsafeRunSync() shouldEqual "Error"
    }
  }

  "redeemWith" should {
    "return IO which contains 8" in {
      example1
        .redeemWith(_ => example2, x => IO(x * 4))
        .unsafeRunSync() shouldEqual 8

    }
    "return IO which contains 3" in {
      example3
        .redeemWith(_ => example2, x => IO(x * 4))
        .unsafeRunSync() shouldEqual 3
    }
  }

  "unsafeToFuture" should {
    "return Future of body value" in {
      Await.result(example1.unsafeToFuture(), 5.seconds) shouldEqual 2
    }
  }

  "apply" should {
    "return IO which contains 2" in {
      IO.apply(2).unsafeRunSync() shouldEqual 2
    }
  }

  "suspend" should {
    "return IO which contains 2" in {
      IO.suspend(IO(2)).unsafeRunSync() shouldEqual 2
    }
  }

  "delay" should {
    "return IO which contains 2" in {
      IO.delay(2).unsafeRunSync() shouldEqual 2
    }
  }

  "pure" should {
    "return IO which contains 2" in {
      IO.delay(2).unsafeRunSync() shouldEqual 2
    }
  }

  "raiseError" should {
    "raise an error" in {
      assertThrows[NullPointerException](
        IO.raiseError(new NullPointerException).unsafeRunSync()
      )
    }
  }

  "fromEither" should {
    "throw a NullPointerException" in {
      assertThrows[NullPointerException](
        IO.fromEither(Left(new NullPointerException))
          .unsafeRunSync()
      )
    }
    "return IO with value" in {
      IO.fromEither(Right(2)).unsafeRunSync() shouldEqual 2
    }
  }

  "fromOption" should {
    "throw a NullPointerException" in {
      assertThrows[NullPointerException](
        IO.fromOption(None)(new NullPointerException)
          .unsafeRunSync()
      )
    }

    "return IO with value" in {
      IO.fromOption(Some(2))(new NullPointerException)
        .unsafeRunSync() shouldEqual 2
    }
  }

  "fromTry" should {
    "throw a NullPointerException" in {
      assertThrows[ArithmeticException](IO.fromTry(Try(4 / 0)).unsafeRunSync())
    }

    "return IO with value" in {
      IO.fromTry(Try(2)).unsafeRunSync() shouldEqual 2
    }
  }

  "none" should {
    "return IO with None in body" in {
      IO.none.unsafeRunSync() shouldEqual None
    }
  }

  "raiseUnless" should {
    "return IO[Unit] when cond is true" in {
      IO.raiseUnless(true)(new NullPointerException)
        .unsafeRunSync() shouldEqual ()
    }

    "raise error if cond is false" in {
      assertThrows[NullPointerException](
        IO.raiseUnless(false)(new NullPointerException)
          .unsafeRunSync() shouldEqual ()
      )
    }
  }

  "raiseWhen" should {
    "return IO[Unit] when cond is false" in {
      IO.raiseWhen(false)(new NullPointerException)
        .unsafeRunSync() shouldEqual ()
    }

    "raise error if cond is true" in {
      assertThrows[NullPointerException](
        IO.raiseWhen(true)(new NullPointerException)
          .unsafeRunSync() shouldEqual ()
      )
    }
  }

  "unlessA" should {
    "return IO[Unit] when cond is false" in {
      IO.unlessA(false)(IO()).unsafeRunSync() shouldEqual ()
    }
    "return IO[Unit] when cond is true" in {
      IO.unlessA(true)(IO(2)).unsafeRunSync() shouldEqual ()
    }
  }

  "whenA" should {
    "return IO[Unit] when cond is false" in {
      IO.whenA(false)(IO()).unsafeRunSync() shouldEqual ()
    }
    "return IO[Unit] when cond is true" in {
      IO.whenA(true)(IO(2)).unsafeRunSync() shouldEqual ()
    }
  }

  "unit" should {
    "return IO[Unit]" in {
      IO.unit.unsafeRunSync() shouldEqual ()
    }
  }
}
