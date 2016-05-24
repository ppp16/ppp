import com.cra.figaro.library.atomic._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.VariableElimination
object Exercises4 {
  def main(args: Array[String]) {
    /*
 * Exercise 4.2
In the same poker game, you observe evidence that one player has a picture
card (king or queen). What’s the probability that the other player has a spade?
 */

    val cards = List("Ace of Spades", "King of Spades", "King of Hearts", "Queen of Spades", "Queen of Hearts")
    val cards2 = List("King of Spades", "King of Hearts", "Queen of Spades", "Queen of Hearts")

    val firstPlayerCard = discrete.Uniform(cards2: _*)
    val secondPlayerCard = Chain(firstPlayerCard, (card: String) => discrete.Uniform(cards.filter(_ != card): _*))
    val secondPlayerSpades = VariableElimination.probability(secondPlayerCard, "Ace of Spades") +
      VariableElimination.probability(secondPlayerCard, "King of Spades") +
      VariableElimination.probability(secondPlayerCard, "Queen of Spades")

    println(secondPlayerSpades) // result 0.625 --> 10/16

    /*
 * Exercise 4.3
Let’s elaborate on the rules of this poker game. The game includes betting.
Here are the betting rules:
Player 1 can bet or pass.
If player 1 bets:
– Player 2 can bet or fold.
If player 1 passes:
– Player 2 can bet or pass.
If player 2 bets:
– Player 1 can bet or fold.
The possible betting outcomes are (1) both players bet; (2) both players pass; (3)
one bets and one passes. If both pass, neither player wins or loses anything. If one
bets and one folds, the player that bets wins $1. If both bet, the player with the
higher card wins $2. If both players have the same rank card, spades beats hearts.
You want to build a probabilistic model of this poker game. The goal of this
model is to help you decide your action at any point in the game, based on a
prediction of what will happen for each of the actions.
 */

    // a What are the variables in the model?
    // c What are the functional forms of these dependencies?
    //val firstPlayerCard = discrete.Uniform(cards2:_*)
    //val secondPlayerCard = Chain(firstPlayerCard, (card: String) => discrete.Uniform(cards.filter(_ != card):_*))
    val firstPlayerBet = CPD(firstPlayerCard,
      // pushed all chances a bit cause we like to bluff aswell
      "Ace of Spades" -> Flip(1),
      "King of Spades" -> Flip(0.85),
      "King of Hearts" -> Flip(0.6),
      "Queen of Spades" -> Flip(0.35),
      "Queen of Hearts" -> Flip(0.1))
    val secondPlayerBet = CPD(firstPlayerBet, secondPlayerCard,
      (false, "Ace of Spades") -> Flip(1),
      (false, "King of Spades") -> Flip(0.95),
      (false, "King of Hearts") -> Flip(0.7),
      (false, "Queen of Spades") -> Flip(0.35),
      (false, "Queen of Hearts") -> Flip(0.1),
      (true, "Ace of Spades") -> Flip(1),
      (true, "King of Spades") -> Flip(0.65),
      (true, "King of Hearts") -> Flip(0.4),
      (true, "Queen of Spades") -> Flip(0.15),
      (true, "Queen of Hearts") -> Flip(0))
    val firstPlayerBet2 = CPD(firstPlayerCard,
      // pushed all chances a bit cause we like to bluff aswell
      "Ace of Spades" -> Flip(1),
      "King of Spades" -> Flip(0.75),
      "King of Hearts" -> Flip(0.3),
      "Queen of Spades" -> Flip(0.15),
      "Queen of Hearts" -> Flip(0))
    // after player one passes and player two bets player one is more unlikely to bet
    val firstPlayerSecondBet = !firstPlayerBet && secondPlayerBet && firstPlayerBet2

    // evalueates the cashchange based on the cards and bets
    val cashChange =
      Apply(firstPlayerCard, secondPlayerCard, firstPlayerBet, secondPlayerBet, firstPlayerSecondBet,
        (cardP1: String, cardP2: String, bet1P1: Boolean,
          bet1P2: Boolean, bet2P1: Boolean) =>
          if (bet1P1 && !bet1P2) 1.0
          else if (!bet1P1 && bet1P2 && !bet2P1) -1.0
          else if (!bet1P1 && !bet1P2) 0.0
          else if ((cardP1 == "Ace of Spades") ||
            ((cardP1 == "King of Spades") && (cardP2 != "Ace of Spades")) ||
            ((cardP1 == "King of Hearts") && (cardP2 != "Ace of Spades") && (cardP2 != "King of Spades")) ||
            ((cardP1 == "Queen of Spades") && (cardP2 != "Queen of Hearts"))) 2.0
          else -2.0)

    // b What are the dependencies between the variables?
    /* variable -> dependencies
firstPlayerCard -> none
secondPlayerCard -> firstPlayerCard
firstPlayerBet -> firstPlayerCard
secondPlayerBet -> firstPlayerBet,secondPlayerCard
firstPlayerSecondBet -> firstPlayerCard,firstPlayerBet,secondPlayerBet
 */

    /*
 * d Which numerical parameters do you know for sure? Which do you have to
estimate or learn from experience?
We know the cards and their distribution but we have to guess the betting behaviour
 */

    /*
 * Exercise 4.4
Write a Figaro program to represent the probabilistic model for this game.
Assume certain values for the parameters that need to be estimated. Use the
program to make the following decisions:
a You’re player 1 and were dealt the king of spades. Should you bet or pass?
b You’re player 2 and were dealt the king of hearts, and your opponent bets.
Should you bet or fold?
c Now, see if you can change the values of the estimated parameters in such a
way that it would change your decision.
 */
          println()
    firstPlayerCard.observe("King of Spades")
    firstPlayerBet.observe(true)
    val meanGain = VariableElimination(cashChange)
    meanGain.start()
    meanGain.stop()
    println("If player one has the King of Spades he could expect to get : " + meanGain.mean(cashChange) + " if he bets")
    firstPlayerBet.observe(false)
    val meanGain2 = VariableElimination(cashChange)
    meanGain2.start()
    meanGain2.stop()   
    println("If player one has the King of Spades he could expect to get : " + meanGain2.mean(cashChange) + " if he doesnt bet") 
    
    firstPlayerCard.unobserve()
    firstPlayerBet.unobserve()
    secondPlayerCard.observe("King of Hearts")
    firstPlayerBet.observe(true)
    secondPlayerBet.observe(true)
    val meanGain3 = VariableElimination(cashChange)
    meanGain3.start()
    meanGain3.stop()       
    println("If player two has the King of Hearts and player one bets he should expect to loose : " + meanGain3.mean(cashChange))
    println("Since player 2 only looses 1 if he doesnt bet that would be the smarter choice")
    println()
    
    val cards3 = List("Ace of Spades", "King of Spades", "King of Hearts", "Queen of Spades")

    val queen = Flip(Beta(1,20))
    val firstPlayerCard2 = If(queen,"Queen of Hearts",discrete.Uniform(cards3: _*))
    print(VariableElimination.probability(firstPlayerCard2, "Queen of Hearts"))

  }
}