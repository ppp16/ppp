import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.library.compound.If
import scala.math.pow
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.library.atomic.discrete.Binomial
import com.cra.figaro.library.atomic.continuous.Uniform
object test2 {
  val temperature = Normal(40, 100)
  val sunnyToday = Flip(0.2)
  val greetingToday = If(sunnyToday,
    Select(0.6 -> "Hello, world!", 0.4 -> "Howdy, universe!"),
    Select(0.2 -> "Hello, world!", 0.8 -> "Oh no, not again"))
  val sunnyTomorrow = If(sunnyToday, Flip(0.8), Flip(0.05))
  val greetingTomorrow = If(sunnyTomorrow,
    Select(0.6 -> "Hello, world!", 0.4 -> "Howdy, universe!"),
    Select(0.2 -> "Hello, world!", 0.8 -> "Oh no, not again"))
  val numSunnyDaysInWeek = Binomial(7, 0.2)
  val temperature2 = Uniform(10, 70)
  def greaterThan50(d: Double) = d > 50

  def main(args: Array[String]): Unit = {
    greetingToday.observe("Hello, world!")
    println(VariableElimination.probability(sunnyToday, true))
    println(VariableElimination.probability(greetingTomorrow, "Hello, world!"))
    greetingToday.unobserve()
    println(VariableElimination.probability(sunnyToday, true))
    println(VariableElimination.probability(greetingTomorrow, "Hello, world!"))

    println(VariableElimination.probability(numSunnyDaysInWeek, 3))
    println(binom(7, 3, 0.2))
    println(Importance.probability(temperature, greaterThan50 _))
    Importance.probability(temperature2, greaterThan50 _)
  }

  def binom(n: Int, k: Int, p: Double) = {
    var nfac: Int = 1
    var kfac = 1
    var nsubkfac = 1
    for (i <- 1 to n) {
      nfac *= i
    }
    for (i <- 1 to k) {
      kfac *= i
    }
    for (i <- 1 to n - k) {
      nsubkfac *= i
    }
    println(nfac)
    println(kfac)
    println(nsubkfac)
    nfac / (kfac * nsubkfac) * pow(p, k) * pow((1 - p), (n - k))
  }
}