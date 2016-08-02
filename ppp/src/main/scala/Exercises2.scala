import com.cra.figaro.language.{ Flip, Select,Chain,Apply }
import com.cra.figaro.library.compound.{If}
import com.cra.figaro.algorithm.factored.VariableElimination

object Exercises2 {

  /**
   * Exercise 2.1
   * Extend the Hello World program to add a variable representing the side of bed
	 * you got out of (right or wrong). If you got out of the wrong side of the bed, the
   * greeting is always Oh no, not again! If you got out of the right side of the bed,
   * the greeting logic is the same as before.
   * 
   * Output before the change:

Todays greeting is "Hello, world!" with probability 0.27999999999999997.
If today's greeting is "Hello, world!", todays weather is sunny with probability 0.4285714285714285.
If today's greeting is "Hello, world!", tomorrow's greeting will be "Hello, world!" with probability 0.3485714285714286.
	 
	 * Output after the change: 

Todays greeting is "Hello, world!" with probability 0.13999999999999999.
If today's greeting is "Hello, world!", todays weather is sunny with probability 0.4285714285714285.
If today's greeting is "Hello, world!", tomorrow's greeting will be "Hello, world!" with probability 0.1742857142857143.

   * as expected the chances for the greetings halve if half of the time you stand up on the wrong side of the bed
   * and the second test didnt change cause this is not effected
   * 
   * Exercise 2.2
   * In the original Hello World program, observe that todays greeting is Oh no,
	 * not again! and query todays weather. Now observe the same evidence and ask
 	 * the same query on your modified program from exercise 1. What happened to
	 * the answer to the query? Can you explain the result intuitively?
   * 
   * Output before the change:
   * 
If today's greeting is "Oh no, not again!", todays weather is sunny with probability 0.0.

   * Output after the change:

If today's greeting is "Oh no, not again!", todays weather is sunny with probability 0.12195121951219512.

	 *
	 * In the normal version you can only get the answer "Oh no, not again!" on rainy days so it is
	 * obvious that it has to rain on this query.
	 * In the changed version you can always get this answer, cause you get it if you wake up on the wrong side,
	 * so after the change it considers that it could be sunny with this query if you wake up on the wrong side.
	 * 
	 * Exercise 2.3
	 * In Figaro, you can use the code x === z as shorthand for
	 * Apply(x, z, (b1: Boolean, b2: Boolean) => b1 === b2
   * In other words, it produces an element whose value is true if the values of its
   * two arguments are equal. Without running Figaro, try to guess what the following
   * two programs will produce:
   * a val x = Flip(0.4)
   * val y = Flip(0.4)
   * val z = x
   * val w = x === z
   * println(VariableElimination.probability(w, true))
   * b val x = Flip(0.4)
   * val y = Flip(0.4)
   * val z = y
   * val w = x === z
   * println(VariableElimination.probability(w, true))
   * Now check your answers by running the Figaro program.
   * 
   * Guess for program a)
   * w is tested for the probability to be true and since w is x===z
   * and since z and x are always equal w is always true
   * Guess for program b)
   * w tests if x and y are equal this time and this means it is true
   * if both coinflips equal. So it should be 0.4*0.4+0.6*0.6=0.52
   * 
   * The estimations were proven right by running the code
   * 
   */


  val sideOfBedToday = Flip(0.5)
  val sideOfBedTomorrow = Flip(0.5)
  val sunnyToday = Flip(0.2)
  val badMood = "Oh no, not again!"
  val greetingTodayOnRightSide = If(sunnyToday,Select(0.6 -> "Hello, world!", 0.4 -> "Howdy, universe!"),
    Select(0.2 -> "Hello, world!", 0.8 -> "Oh no, not again!"))
    
  val greetingToday = Apply(sideOfBedToday, greetingTodayOnRightSide, (b: Boolean, s: String) => if(b)badMood else  s)

  val sunnyTomorrow = If(sunnyToday, Flip(0.8), Flip(0.05))
  val greetingTomorrowOnRightSide = If(sunnyTomorrow,
    Select(0.6 -> "Hello, world!", 0.4 -> "Howdy, universe!"),
    Select(0.2 -> "Hello, world!", 0.8 -> "Oh no, not again!"))
    
  val greetingTomorrow = Apply(sideOfBedTomorrow, greetingTomorrowOnRightSide, (b: Boolean, s: String) => if(b)badMood else  s)
  
  def predict() {
    val result = VariableElimination.probability(greetingToday,"Hello, world!")
    println("Todays greeting is \"Hello, world!\" " +
      "with probability " + result + ".")
  }
  
  def infer() {
    greetingToday.observe("Oh no, not again!")
    val result = VariableElimination.probability(sunnyToday, true)
    println("If today's greeting is \"Oh no, not again!\", todays " +
      "weather is sunny with probability " + result + ".")
  }
  
  def learnAndPredict() {
    greetingToday.observe("Hello, world!")
    val result = VariableElimination.probability(greetingTomorrow,
      "Hello, world!")
    println("If today's greeting is \"Hello, world!\", " +
      "tomorrow's greeting will be \"Hello, world!\" " +
      "with probability " + result + ".")
  }
  
  // for exercise 2.1
//  def predict() {
//    val result = VariableElimination.probability(greetingToday,"Hello, world!")
//    println("Todays greeting is \"Hello, world!\" " +
//      "with probability " + result + ".")
//  }
//  
//  def infer() {
//    greetingToday.observe("Hello, world!")
//    val result = VariableElimination.probability(sunnyToday, true)
//    println("If today's greeting is \"Hello, world!\", todays " +
//      "weather is sunny with probability " + result + ".")
//  }
//  
//  def learnAndPredict() {
//    greetingToday.observe("Hello, world!")
//    val result = VariableElimination.probability(greetingTomorrow,
//      "Hello, world!")
//    println("If today's greeting is \"Hello, world!\", " +
//      "tomorrow's greeting will be \"Hello, world!\" " +
//      "with probability " + result + ".")
//  }
  def main(args: Array[String]) {
    predict()
    infer()
    learnAndPredict()
    
//val x = Flip(0.4)
//val y = Flip(0.4)
//val z = y
//val w = x === z
//println(VariableElimination.probability(w, true))
//val x = Flip(0.4)
//val y = Flip(0.4)
//val z = y
//val w = x === z
//println(VariableElimination.probability(w, true))
  }

}