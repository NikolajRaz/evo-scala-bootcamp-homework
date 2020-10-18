package error_handling
import cats.data.ValidatedNec
import cats.syntax.all._
import java.time.LocalDate

object ErrorHandlingHomework {

  case class PaymentCard(name: String, number: String, expirationDate: String, securityCode: String)

  sealed trait ValidationError

  object ValidationError {

    final case object NameFormatIsInvalid extends ValidationError {
      override def toString: String = "Format is invalid, should be - Name Surname"
    }

    final case object CardNumberFormatIsInvalid extends ValidationError {
      override def toString: String = "Card number should content only numbers"
    }

    final case object CardNumberLengthIsInvalid extends ValidationError {
      override def toString: String = "Card number length is invalid"
    }

    final case object ExpirationDateIsInvalid extends ValidationError {
      override def toString: String = "Expiration date format is invalid"
    }

    final case object CardIsExpired extends ValidationError {
      override def toString: String = "Card is expired"
    }

    final case object SecurityCodeFormatIsInvalid extends ValidationError {
      override def toString: String = "Security code format is invalid"
    }

    final case object SecurityCodeLengthIsInvalid extends ValidationError {
      override def toString: String = "Security code length is invalid"
    }

  }

  object PaymentCardValidator {

    import ValidationError._

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validate(
                  name: String,
                  number: String,
                  expirationDate: String,
                  securityCode: String,
                ): AllErrorsOr[PaymentCard] = {
      def validateName: AllErrorsOr[String] = {
        val nameSurname = name.split(" ")
        if (nameSurname.length != 2)
          NameFormatIsInvalid.invalidNec
        else if (!nameSurname(0).matches("^[a-zA-Z]+$") || !nameSurname(1).matches("^[a-zA-Z]+$"))
          NameFormatIsInvalid.invalidNec
        else name.validNec
      }

      def validateCardNumber: AllErrorsOr[String] = {
        val numberWithoutSpaces = number.replaceAll(" ", "")
        if (!numberWithoutSpaces.matches("^[0-9]+$"))
          CardNumberFormatIsInvalid.invalidNec
        else if (numberWithoutSpaces.length != 16)
          CardNumberLengthIsInvalid.invalidNec
        else numberWithoutSpaces.validNec
      }

      def validateDate: AllErrorsOr[String] = {
        val currentDate = LocalDate.now.toString.split("-")
        val currentYear = currentDate(0).slice(0, 2)
        val currentMonth = currentDate(1)
        val splittedDate = expirationDate.split("/")
        if (splittedDate.length != 2)
          ExpirationDateIsInvalid.invalidNec
        else if (splittedDate(0).toIntOption.isEmpty || splittedDate(1).toIntOption.isEmpty)
          ExpirationDateIsInvalid.invalidNec
        else if (splittedDate(0).toInt < 1 || splittedDate(0).toInt > 12 || splittedDate(0).length != 2 || splittedDate(1).length != 2)
          ExpirationDateIsInvalid.invalidNec
        else if ((splittedDate(0).toInt < currentMonth.toInt && splittedDate(1) == currentYear) || (splittedDate(1) < currentYear))
          CardIsExpired.invalidNec
        else expirationDate.validNec
      }

      def validateCode: AllErrorsOr[String] = {
        if (securityCode.toIntOption.isEmpty)
          SecurityCodeFormatIsInvalid.invalidNec
        else if (securityCode.length != 3)
          SecurityCodeLengthIsInvalid.invalidNec
        else securityCode.validNec
      }

      (validateName, validateCardNumber, validateDate, validateCode).mapN(PaymentCard)
    }
  }
}