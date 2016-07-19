import com.cra.figaro.library.atomic._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling._
import scala.collection.mutable.ListBuffer
import scalaz.syntax.ToIdOps
import com.cra.figaro.language.Universe._
import com.cra.figaro.algorithm.filtering.ParticleFilter

object Exercise8 {

  /*

One in a thousand cars produced by the factory that made your car is faulty. On
any given day, the fuel efficiency of a car is either high or low, and follows the
following distribution: If the previous day had low fuel efficiency, then the current
day’s fuel efficiency is low with probability 90%. If the previous day had
high fuel efficiency, then the current day’s fuel efficiency is low with probability
90% for faulty cars and 5% for normal cars. Your car also has a fuel-economy
gauge that shows whether your car has high or low fuel efficiency that day. The
gauge shows the correct value with probability 0.95 and the incorrect value with
probability 0.05.

a Draw a dynamic Bayesian network to represent this system.

Faulty  -->  Fuel Efficiency(1)
Faulty  -->  Fuel Efficiency(2)
Faulty  -->  Fuel Efficiency(3)
...
Faulty  -->  Fuel Efficiency(i) | i > 0 
Fuel Efficiency(0)  -->  Fuel Efficiency(1)
Fuel Efficiency(1)  -->  Fuel Efficiency(2)
Fuel Efficiency(2)  -->  Fuel Efficiency(3)
...
Fuel Efficiency(i)  -->  Fuel Efficiency(i+1) | i >= 0

Fuel Efficiency(0)  -->  Fuel Gauge(0)
Fuel Efficiency(1)  -->  Fuel Gauge(1)
Fuel Efficiency(2)  -->  Fuel Gauge(2)
...
Fuel Efficiency(i)  -->  Fuel Gauge(i) | i >= 0


--------------------				
|      Faulty      |------------------------------- \ -------------------------------------------
--------------------			           |     		 		  /								|														|
          													 |							\								|														|
          													 V							/							  V														V
--------------------	  	 --------------------  		\	 		 --------------------	  	  ----------------------		
|Fuel Efficiency(0)|  -->  |Fuel Efficiency(1)|	--> / -->	 |Fuel Efficiency(i)|  -->	|Fuel Efficiency(i+1)|
--------------------       --------------------  		\ 		 --------------------	  		----------------------	
          |													 |							/								|														|
          V													 V							\								V														V
--------------------  		 --------------------			/			 --------------------				----------------------	
|  Fuel Gauge(0)   |	-->  |  Fuel Gauge(1)   |	-->	\	-->	 |  Fuel Gauge(i)   |	 -->	|  Fuel Gauge(i+1)   |
--------------------   	   --------------------			/			 --------------------				----------------------	



b Write a Figaro program to represent this system.

   */
    val init = Universe.createNew()
    val faulty = Flip(0.001)("faulty",init)
    val efficiency = Constant("high")("efficiency", init)
  
    val gauge = CPD(efficiency,
      ("low") -> Select(0.95 -> "low", 0.05 -> "high"),
      ("high") -> Select(0.95 -> "high", 0.05 -> "low"))("gauge", init)
  
    def processRound(actual: Universe): (Element[(String, String)]) = {
      val nextEfficiency = CPD(actual.get[String]("efficiency"), actual.get[Boolean]("faulty"),
        ("low", true) -> Select(0.90 -> "low", 0.10 -> "high"),
        ("low", false) -> Select(0.90 -> "low", 0.10 -> "high"),
        ("high", true) -> Select(0.90 -> "low", 0.10 -> "high"),
        ("high", false) -> Select(0.95 -> "high", 0.05 -> "low"))
      val nextGauge = CPD(actual.get[String]("efficiency"),
        ("low") -> Select(0.95 -> "low", 0.05 -> "high"),
        ("high") -> Select(0.95 -> "high", 0.05 -> "low"))
      ^^(nextEfficiency, nextGauge)
    }
  
    def nextUniverse(actual: Universe): Universe = {
      val next = Universe.createNew()
      val state = processRound(actual)
      Apply(actual.get[Boolean]("faulty"),(s: (Boolean)) => s)("faulty",next)
      Apply(state, (s: (String, String)) => s._1)("efficiency", next)
      Apply(state, (s: (String, String)) => s._2)("gauge", next)
      next
    }

    def nextUniverse2(actual: Universe): Universe = {
      val next = Universe.createNew()
      val state = processRound(actual)
      val change = Apply(Select(0.99 -> true, 0.01 -> false),(t:(Boolean)) => t)
      Apply(actual.get[Boolean]("faulty"),(s: (Boolean)) => if(change.generateValue()) s else !s )("faulty",next)
      Apply(state, (s: (String, String)) => s._1)("efficiency", next)
      Apply(state, (s: (String, String)) => s._2)("gauge", next)
      next
    }

  def main(args: Array[String]) {

  /*
c  Create two observation sequences of length 100 from this program, one for a
normal car and one for a faulty car.
*/
      println("------------------------------------------------------------")
      faulty.observe(false)
      var sample1: List[String] = Nil
      var actual = init
      for (i <- 0 to 99) {
        actual = nextUniverse(actual)
        actual.get[String]("gauge").generateValue()
        val gauge = actual.get[String]("gauge")
        val alg = Importance(1, gauge)
        alg.start()
        val obs: String = alg.distribution(gauge)(0)._2
        sample1 = sample1 ::: List(obs)
      }
      println("Sample for 100 runs with a non-faulty car")
      println()
      println(sample1)
      println("Number of 'high's: " + sample1.count(_.equals("high")))
      println("Number of 'low's: " + sample1.count(_.equals("low")))
      faulty.unobserve()
  
      println("------------------------------------------------------------")
      faulty.observe(true)
      var sample2: List[String] = Nil
      actual = init
      println()
      for (i <- 0 to 99) {
        actual = nextUniverse(actual)
        actual.get[String]("gauge").generateValue()
        val gauge = actual.get[String]("gauge")
        val alg = Importance(1, gauge)
        alg.start()
        val obs: String = alg.distribution(gauge)(0)._2
        sample2 = sample2 ::: List(obs)
      }
      println("Sample for 100 runs with a non-faulty car")
      println()
      println(sample2)
      println("Number of 'high's: " + sample2.count(_.equals("high")))
      println("Number of 'low's: " + sample2.count(_.equals("low")))
      faulty.unobserve()
      
 /* 
 d Use particle filtering to monitor the state of this system. Run the particle filter
on your two observation sequences. Use 100 samples. You’ll probably find
that most of the time, you cannot detect a faulty car. Why do you think this is?
  */   
      println("------------------------------------------------------------")
      println("Particlefilter with non-faulty car and a samplesize of 100")
      println()
      actual = init
      for (i <- 0 to 29) {
      val pf1 = ParticleFilter(actual, nextUniverse, 100)
      pf1.start()
      for { i <- 0 until 100 } {
        pf1.advanceTime(List(NamedEvidence("gauge",
          Observation(sample1(i)))))
      }
      print(pf1.currentProbability("faulty", true) + "  ")
      }
      
      println("------------------------------------------------------------")
      println("Particlefilter with faulty car and a samplesize of 100")
      println()
      actual = init
      println()
      for (i <- 0 to 29) {
      val pf2 = ParticleFilter(actual, nextUniverse, 100)
      pf2.start()
      for { i <- 0 until 100 } {
        pf2.advanceTime(List(NamedEvidence("gauge",
          Observation(sample2(i)))))
      }
      print(pf2.currentProbability("faulty", true) + "  ")
      }
      
/*
 e Now repeat part (d) with 10,000 samples. You should usually get a different
result. Can you explain why? (Make sure to run the experiment multiple
times—particle filtering will not always produce the same answer.)
 */
      
      println("------------------------------------------------------------")
      println("Particlefilter with non-faulty car and a samplesize of 10000")
      println()
      actual = init
      println()
      for (i <- 0 to 9) {
      val pf1 = ParticleFilter(actual, nextUniverse, 10000)
      pf1.start()
      for { i <- 0 until 100 } {
        pf1.advanceTime(List(NamedEvidence("gauge",
          Observation(sample1(i)))))
      }
      print(pf1.currentProbability("faulty", true) + "  ")
      }
      
      println("------------------------------------------------------------")
      println("Particlefilter with faulty car and a samplesize of 10000")
      println()
      actual = init
      println()
      for (i <- 0 to 9) {
      val pf2 = ParticleFilter(actual, nextUniverse, 10000)
      pf2.start()
      for { i <- 0 until 100 } {
        pf2.advanceTime(List(NamedEvidence("gauge",
          Observation(sample2(i)))))
      }
      print(pf2.currentProbability("faulty", true) + "  ")
      }       
      
      /*
Now let’s change the model of the previous exercise slightly, in an attempt to
make particle filtering run more smoothly. A faulty car doesn’t always stay
faulty; it has some probability to correct itself at each time step. Likewise, a normal
car could become faulty. Specifically, a car maintains the same faulty state
from one time step to the next with probability 0.99, and flips state with probability
0.01.
Run particle filtering using this model with 100 samples.
What are the differences between your new results and the
previous ones? Repeat the experiment with 10,000 samples.
Evaluate the trade-off.
       */
      
      println("------------------------------------------------------------")
      println("Particlefilter with non-faulty car and a samplesize of 100")
      println()
      actual = init
      println()
      for (i <- 0 to 29) {
      val pf1 = ParticleFilter(actual, nextUniverse2, 100)
      pf1.start()
      for { i <- 0 until 100 } {
        pf1.advanceTime(List(NamedEvidence("gauge",
          Observation(sample1(i)))))
      }
      print(pf1.currentProbability("faulty", true) + "  ")
      }
      
      println("------------------------------------------------------------")
      println("Particlefilter with faulty car and a samplesize of 100")
      println()
      actual = init
      println()
      for (i <- 0 to 29) {
      val pf2 = ParticleFilter(actual, nextUniverse2, 100)
      pf2.start()
      for { i <- 0 until 100 } {
        pf2.advanceTime(List(NamedEvidence("gauge",
          Observation(sample2(i)))))
      }
      print(pf2.currentProbability("faulty", true) + "  ")
      }   
            
      println("------------------------------------------------------------")
      println("Particlefilter with non-faulty car and a samplesize of 10000")
      println()
      actual = init
      println()
      for (i <- 0 to 9) {
      val pf1 = ParticleFilter(actual, nextUniverse2, 10000)
      pf1.start()
      for { i <- 0 until 100 } {
        pf1.advanceTime(List(NamedEvidence("gauge",
          Observation(sample1(i)))))
      }
      print(pf1.currentProbability("faulty", true) + "  ")
      }
      
      println("------------------------------------------------------------")
      println("Particlefilter with faulty car and a samplesize of 10000")
      println()
      actual = init
      println()
      for (i <- 0 to 9) {
      val pf2 = ParticleFilter(actual, nextUniverse2, 10000)
      pf2.start()
      for { i <- 0 until 100 } {
        pf2.advanceTime(List(NamedEvidence("gauge",
          Observation(sample2(i)))))
      }
      print(pf2.currentProbability("faulty", true) + "  ")
      }   
      println("------------------------------------------------------------")
  }

}