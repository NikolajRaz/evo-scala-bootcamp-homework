package basics

object FirstHomework {
  // Homework. Implement functions that calculate https://en.wikipedia.org/wiki/Lowest_common_denominator and
  // https://en.wikipedia.org/wiki/Greatest_common_divisor for integers.

  def lcm(a: Int, b: Int): Int = {
    if (a == 0 || b == 0)
      return 0
    (a*b).abs/gcd(a, b)
  }

  def gcd(a: Int, b: Int, isItFirstIteration: Boolean = true): Int = {
    if (a==0 || (b==0 && isItFirstIteration))
      return 0
    if (b==0)
      a.abs
    else
      gcd(b, a%b, isItFirstIteration = false)
  }
}