import com.cra.figaro.library.atomic._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.VariableElimination
import scala.collection.mutable.ListBuffer
import scalaz.syntax.ToIdOps
import com.cra.figaro.language.Universe._

object Exercise6 {
  val aliceWinChance = Flip(0.52);

  //  def processRound(previous: Universe){
  //    val previous = universe;
  //    universe = Universe.createNew();
  //    val aliceWin = aliceWinChance;
  //    val a = Apply(previous.get[Int]("alice"),aliceWin,(x:Int, win:Boolean) => if(win)x+1 else x);
  //    val b = Apply(previous.get[Int]("bob"),aliceWin,(x:Int, win:Boolean) => if(win)x else x+1);
  //    Constant(a)("alice",universe);
  //    Constant(b)("bob",universe);
  //  }

  def isGameOver(universe: Universe) = {
    Apply(universe.get[Element[Int]]("alice").generateValue(), universe.get[Element[Int]]("bob").generateValue(), (a: Int, b: Int) => a >= 21 || b >= 21).generateValue()
  }

  def bobWon(universe: Universe) = {
    Apply(universe.get[Int]("alice"), universe.get[Int]("bob"), (a: Int, b: Int) => a < b).generateValue()
  }

  def main(args: Array[String]) {
    Universe.createNew()
    Constant(0)("alice", universe)
    Constant(0)("bob", universe)
    
    print(universe.get[Int]("bob").generateValue())

    //  while(!isGameOver){ 
    //    processRound(universe)
    //      universe = nextUniverse(universe)
    //  }
    //  println(bobWon(universe))
    
    Universe.createNew()
    val aliceWinChance = Flip(Beta(2,2));
    
    Constant(11)("alice", universe)
    Constant(8)("bob", universe)
    
    print(universe.get[Int]("bob").generateValue())

    //  while(!isGameOver){ 
    //    processRound(universe)
    //      universe = nextUniverse(universe)
    //  }
    //  println(bobWon(universe))
    
  }

  def processRound(alice: Element[Int], bob: Element[Int]): (Element[(Int, Int)]) = {
    val aliceWin = aliceWinChance;
    val a = Apply(alice, aliceWin, (x: Int, win: Boolean) => if (win) x + 1 else x);
    val b = Apply(bob, aliceWin, (x: Int, win: Boolean) => if (win) x else x + 1);
    ^^(a, b)
  }

  def nextUniverse(previous: Universe): Universe = {
    val next = Universe.createNew()
    val a = previous.get[Element[Int]]("alice")
    val b = previous.get[Element[Int]]("bob")
    val state = processRound(a.generateValue(), b.generateValue())
    Apply(state, (s: (Int, Int)) => s._1)("bob", next)
    Apply(state, (s: (Int, Int)) => s._2)("alice", next)
    next
  }

}