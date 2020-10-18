package error_handling

import cats.syntax.all._
import error_handling.ErrorHandlingHomework._
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ErrorHandlingHomeworkSpec
  extends AnyFlatSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks {
  "PaymentValidator" should "handle valid and invalid " in {
    import ValidationError._

    PaymentCardValidator.validate(
      name = "Nikolajs Razins",
      number = "4444 4444 4444 4444",
      expirationDate = "04/22",
      securityCode = "777"
    ) shouldBe PaymentCard("Nikolajs Razins", "4444444444444444", "04/22","777").validNec

    def checkInvalid(name: String, number: String, expirationDate: String, securityCode: String, errors: Set[ValidationError]): Assertion =
      PaymentCardValidator.validate(
        name = name,
        number = number,
        expirationDate = expirationDate,
        securityCode = securityCode
      ).leftMap(_.toList.toSet) shouldBe errors.invalid

    checkInvalid(
      name = "NikolajsRazins",
      number = "4444 4444 4444 4444",
      expirationDate = "10/22",
      securityCode = "777",
      errors = Set(NameFormatIsInvalid),
    )
    checkInvalid(
      name = "Nikolajs Razi4ns",
      number = "4444 4444 44D4 4444",
      expirationDate = "10/N2",
      securityCode = "777",
      errors = Set(NameFormatIsInvalid, CardNumberFormatIsInvalid, ExpirationDateIsInvalid),
    )
    checkInvalid(
      name = "Nikolajs Razins",
      number = "4444 4444 4444 44444",
      expirationDate = "13/22",
      securityCode = "777",
      errors = Set(CardNumberLengthIsInvalid, ExpirationDateIsInvalid),
    )
    checkInvalid(
      name = "Nikolajs Razins",
      number = "4444 4444 4444 4444",
      expirationDate = "09/20",
      securityCode = "7778",
      errors = Set(CardIsExpired, SecurityCodeLengthIsInvalid),
    )
    checkInvalid(
      name = "Nikolajs Razins",
      number = "4444 4444 4444 4444",
      expirationDate = "10/19",
      securityCode = "777",
      errors = Set(CardIsExpired),
    )
  }
}