import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.library.compound.If
import scala.math.pow
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.library.atomic.discrete.Binomial
import com.cra.figaro.library.atomic.continuous.Uniform

object Exercises1 {

  /**
   * Exercise 1.1
   * Imagine that you want to use a probabilistic reasoning system to reason about
   * the outcome of poker hands.
   *
   * a) What kind of general knowledge could you encode in your model?
   * Knowledge of the deck - Number of cards, values, colors
   * Rules of Poker - Which combinations are there, which beats which, whats the gameflow
   * Knowledge of Playing pattern - How do people play in general
   *
   * b) Describe how you might use the system to predict the future. What’s the evidence?
   * What’s the query?
   * You could for example calculate how you have to bluff to make enemies fold, or how much the opponent will most probably bet.
   * The Queries would be how high i have to bluff or what will he bet.
   * The Evidence would be the cards on the table, the cards gone/folded,the bets, your hand and everything you know about the other player.
   *
   * c) Describe how you might use the system to infer past causes of current observations.
   * What’s the evidence? What’s the query?
   * You can infer what cards an opponent might have based on observations.
   * The Query would be which cards the opponent has/had.
   * The Evidence would be the cards on the table, the cards gone/folded,the bets, your hand and everything you know about the other player.
   *
   * d) Describe how the inferred past causes can help you with your future predictions.
   * If you know his cards through your inferences you can predict all of his upcoming bets and can get the most out of the round.
   
   * Excercise 1.2
   * In the Hello World example, change the probability that today’s weather is sunny
	 * according to the following table. How do the outputs of the program change?
   * Why do you think they change this way?
   * 
   * The Outcomes of sunnyToday = 0.2[0.1] are
   * Today’s greeting is "Hello, world!" with probability 0.28[0.24].
	 * If today's greeting is "Hello, world!", today’s weather is sunny with probability 0.43[0.25].
	 * If today's greeting is "Hello, world!", tomorrow's greeting will be "Hello, world!" with probability 0.35[0.295].
   * 
   * Today greeting "Hellow World!": sunnyToday*0.6+(1-sunnyToday)*0.2=0.06+0.18=0.24
   * 
   * "Hellow World!" -> todays sunny: This is computed with the VariableElimination algorithm 
   * 		which will be in detail explained in chapter 10 but to explain it roughly it takes one variable
   * 		and eliminates it in the equation  and since the sunnyToday chance has a big influence on the
   * 		"Hellow, world! answer it chances the probability that drastically if u halve the chance
   * 
   * Today "Hello, world!" -> tomorrow "Hello, world!": It operates similar to the one above
   * 		with the change that the elimination is used on the greeting tomorrow and since the
   * 		influence of sunnyToday is not that high on the greetingTomorrow it does not chance 
   * 		as drastically as above.
   * 
   * Exercise 1.3
   * Modify the Hello World example to add a new greeting: “Hi, galaxy!” Give this
	 * greeting some probability when the weather is sunny, making sure to reduce
   * the probability of the other greetings so the total probability is 1. Also, modify the
   * program so that all the queries print the probability of “Hi, galaxy!” instead of
   * “Hello, world!” Try to do this for both the Java and Figaro versions of the Hello
   * World program. Compare the process for the two languages.
   * 
   * The output of both changed codes are:
   * 
   * 
Today's greeting is Hi, Galaxy!with probability 0.1.
If today's greeting is Hi, Galaxy!, today's weather is sunny with probability 1.0.
If today's greeting is Hi, Galaxy!, tomorrow's greeting will be Hi, Galaxy! with probability 0.4

   * Process of changing the figaro code:
   * 
  	Changing twice
    		Select(0.6 -> "Hello, world!", 0.4 -> "Howdy, universe!"),
==>    	Select(0.3 -> "Hello, world!",0.5 -> "Hi, Galaxy!", 0.2 -> "Howdy, universe!"),

    and in the Methods predict(),infer() and learnAndPredict() exchange every instance of
    		Hello, world!
==>  		Hi, Galaxy!

   * Process of changing the java code:
   * 
   	 Assign new variables
	static String greeting4 = "Hi, Galaxy!";
	static Double pGreeting4TodayIfSunnyToday = 0.5;
	static Double pGreeting4TomorrowIfSunnyTomorrow = 0.5;

		Change other variables
	static Double pGreeting1TodayIfSunnyToday = 0.6;
	static Double pGreeting2TodayIfSunnyToday = 0.4;
	static Double pGreeting1TomorrowIfSunnyTomorrow = 0.3;
	static Double pGreeting2TomorrowIfSunnyTomorrow = 0.2;
	
	and in the methods you have to hardcode each change manually since the whole project is not made for dynamic changes
	
	* The changes to make were far more simple in the figaro code, escpecially since it uses a predefined algorithm
	* to calculate the outcomes and in the javacode you have to hardcode the chances by hand in the methods.
	* But if the javacode would be more advances the changes there could be as easy as it were in figaro.
	* But thats what makes figaro so useful for these subjects, the availability of those algorithm and predefined methods.
	* 
   * */

  val sunnyToday = Flip(0.2)
  val greetingToday = If(sunnyToday,
    Select(0.3 -> "Hello, world!",0.5 -> "Hi, Galaxy!", 0.2 -> "Howdy, universe!"),
    Select(0.2 -> "Hello, world!", 0.8 -> "Oh no, not again"))
  val sunnyTomorrow = If(sunnyToday, Flip(0.8), Flip(0.05))
  val greetingTomorrow = If(sunnyTomorrow,
    Select(0.3 -> "Hello, world!",0.5 -> "Hi, Galaxy!", 0.2 -> "Howdy, universe!"),
    Select(0.2 -> "Hello, world!", 0.8 -> "Oh no, not again"))
    
  def predict() {
    val result = VariableElimination.probability(greetingToday,
      "Hi, Galaxy!")
    println("Today’s greeting is \"Hi, Galaxy!\" " +
      "with probability " + result + ".")
  }
  
  def infer() {
    greetingToday.observe("Hi, Galaxy!")
    val result = VariableElimination.probability(sunnyToday, true)
    println("If today's greeting is \"Hi, Galaxy!\", today’s " +
      "weather is sunny with probability " + result + ".")
  }
  
  def learnAndPredict() {
    greetingToday.observe("Hi, Galaxy!")
    val result = VariableElimination.probability(greetingTomorrow,
      "Hi, Galaxy!")
    println("If today's greeting is \"Hi, Galaxy!\", " +
      "tomorrow's greeting will be \"Hi, Galaxy!\" " +
      "with probability " + result + ".")
  }
  def main(args: Array[String]) {
    predict()
    infer()
    learnAndPredict()
  }
}