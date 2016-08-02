package programmingProject

import javaCode.StartClient2
import com.cra.figaro.util.random
import com.cra.figaro.library.atomic._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.library.collection._
import com.cra.figaro.algorithm.filtering.ParticleFilter
import com.cra.figaro.algorithm.sampling.MetropolisHastings
import com.cra.figaro.algorithm.sampling.ProposalScheme
import java.io.PrintStream
import scala.util.Random
import com.cra.figaro.algorithm.sampling.Importance

object Main {

  // Settings
  val maxIterations = 100
  var FLAG = 0
  var ROW = 3
  var COL = 5
  var MIN = 82
  var TRESHOLD = 100
  
  //  val init = initializeUniverse()
  var graph = new Graph()
  val graph2 = new Graph()
  graph.generateAutobahn()
  graph2.generateRandomGraph(ROW, COL)
  //  var cost = 0

  
  def main(args: Array[String]) {
    val sampleSize = 1000
    var samples = generateSample(sampleSize, travelingTo)
    println("Results for choosing completly random: ")
    process(samples, sampleSize)
    //  Average costs: 2437.65806838632273
    //Average turns: 10.698460307938412
    //Probability to reach within a cost of 99: 0.2006
    //Probability to reach in minimal cost(82): 0.0288

    FLAG = 1
    samples = generateSample(sampleSize, travelingTo)
    println()
    println("Results for not going back but still random: ")
    process(samples, sampleSize)
    //Average costs: 302.8181818181818
    //Average turns: 14.412587412587413
    //Probability to reach within a cost of 99: 0.147
    //Probability to reach in minimal cost(82): 0.003

    FLAG = 2
    samples = generateSample(sampleSize, travelingTo)
    println()
    println("Results for most likely traversing edges with lower costs: ")
    process(samples, sampleSize)
    //Average costs: 284.8061938061938
    //Average turns: 13.550449550449551
    //Probability to reach within a cost of 99: 0.165
    //Probability to reach in minimal cost(82): 0.006

    FLAG = 3
    samples = generateSample(sampleSize, travelingTo)
    println()
    println("Results for considering distances: ")
    process(samples, sampleSize)
    //Average costs: 101.30913817236552
    //Average turns: 4.035992801439712
    //Probability to reach within a cost of 99: 0.7644
    //Probability to reach in minimal cost(82): 0.2982

    FLAG = 4
    samples = generateSample(sampleSize, travelingTo)
    println()
    println("Results for considering costs and distances: ")
    process(samples, sampleSize)
    //Average costs: 106.33213357328535
    //Average turns: 4.781643671265747
    //Probability to reach within a cost of 99: 0.701
    //Probability to reach in minimal cost(82): 0.118
    
    graph = graph2
    samples = generateSample(sampleSize, travelingToB)
    
    val costs = samples.map((i: Element[(Int, Int)]) => Apply(i, (s: (Int, Int)) => s._1).generateValue())
    MIN = costs.reduceLeft(_ min _)
    TRESHOLD = MIN
    println()
    println("Results for choosing completly random: ")
    process(samples, sampleSize)


    FLAG = 1
    samples = generateSample(sampleSize, travelingToB)
    println()
    println("Results for not going back but still random: ")
    process(samples, sampleSize)
    
    FLAG = 2
    samples = generateSample(sampleSize, travelingToB)
    println()
    println("Results for most likely traversing edges with lower costs: ")
    process(samples, sampleSize)

    FLAG = 3
    samples = generateSample(sampleSize, travelingToB)
    println()
    println("Results for considering distances: ")
    process(samples, sampleSize)

    FLAG = 4
    samples = generateSample(sampleSize, travelingToB)
    println()
    println("Results for considering costs and distances: ")
    process(samples, sampleSize)
    
    //    setDestination(30,30,init)
    //    Constant(30)("goalX",init)
    //    Constant(30)("goalY",init)
    //    print(x)
    //    var board: Array[Array[Int]] = Array.ofDim[Int](WIDTH , HEIGHT)
    //    val t = travelingTo(30, 30)
    //    val algorithm = Importance(10000, travelingTo(30, 30))
    //algorithm.start()
    //println(algorithm.probability(travelingTo(30, 30), (i: Int) => i < 10))
    ////println(algorithm.distribution(rembrandt2).toList)
    //algorithm.kill()
    //      var actual = Universe.createNew()
    //      // start at Ludwigsburg
    //      Constant(10)("xpos",actual)
    //      Constant(3)("ypos",actual)
    //      Constant(0)("costs",actual)
    //      Constant(false)("end",actual)
    //      Constant(30)("goalx",actual)
    //      Constant(30)("goaly",actual)
    //      
    //      val alg = ParticleFilter(actual, nextUniverse, 1000)
    //      alg.start()
    //      val evidence = List(NamedEvidence("costs", Observation(cost)))
    //      for(i <- 0 to 100)
    //      alg.advanceTime(evidence)
    //print(alg.currentExpectation("costs", (l: Int) => l))
    //
    //      print(alg.currentProbability("costs", 84) + "  ")
    //      var count = Apply(travelingTo(30, 30),(i:Int) => i.toDouble)
    //val alg = Importance(10000, count)
    //alg.start()
    //println(alg.mean(count))
    //      val count2 = Apply(travelingTo(30, 30),(i:Int) => i.toDouble)
    //val alg2 = Importance(10000, count2)
    //alg2.start()
    //println(alg2.mean(count2))

    //    var count =Apply(generateSample(1,travelingTo).head,(s:(Int,Int)) => s._1)
    //    val algs = for (i <- 0 to 100) yield Importance(100, count)
    //    algs.foreach(_.start())
    //    val t = algs.foreach(_.mean(Apply(count,(i:Int) => i.toDouble)))
    //    println(t)
    //    val mean = algs.map(print(_))

    //      .map(_.probability(count, 84)) 
    //      print(mean)

    //  val samples = generateSample(1000, travelingTo)
    //  val costs = samples.map((i:Element[(Int,Int)]) => Apply(i, (s: (Int,Int)) => s._1))
    //  val turns = samples.map((i:Element[(Int,Int)]) => Apply(i, (s: (Int,Int)) => s._2))
    //  val a = Importance(1000,costs.head)
    //  a.start()
    //  Thread.sleep(1000)
    //  a.stop()
    //  print(a.probability(costs.head,(i:Int) => i < 100))
    //  a.kill()

    //        var actual = Universe.createNew()
    //      // start at Ludwigsburg
    //      Constant(10)("xpos",actual)
    //      Constant(3)("ypos",actual)
    //      Constant(0)("costs",actual)
    //      Constant(0)("turns",actual)
    //      Constant(false)("end",actual)
    //      Constant(30)("goalx",actual)
    //      Constant(30)("goaly",actual)
    //      val pf1 = ParticleFilter(actual, nextUniverse, 1000)
    //      var sample: List[Int] = 18 :: 53 :: 82 :: 82 :: 82 :: Nil
    //      pf1.start()
    //      
    //      for { i <- 0 until 5 } {
    //        pf1.advanceTime(List(NamedEvidence("costs",Observation(sample(i)))))
    //      }
    //
    //      print(pf1.currentProbability("costs", 82) + "  ")
    //      
    //  
    //    val test = new StartClient2();
    //    Thread.sleep(500)
    //    test.translate(1)
    //    while(true){
    //      test.translate(-1)
    //      Thread.sleep(100)
    //      test.translate(1)
    //      Thread.sleep(100)
    //    }
  }

  var lastEdge: Edge = null
  def nextUniverse(actual: Universe): Universe = {

    val next = Universe.createNew()

    // check if goal is reached
    try {
      val reachedGoal = Apply(actual.get[Int]("xpos"), actual.get[Int]("ypos"),
        actual.get[Int]("goalx"), actual.get[Int]("goaly"),
        (x1: Int, y1: Int, x2: Int, y2: Int) => (x1 == x2) && (y1 == y2))

      Apply(actual.get[Int]("goalx"), (s: Int) => s)("goalx", next)
      Apply(actual.get[Int]("goaly"), (s: Int) => s)("goaly", next)

      if (reachedGoal.generateValue()) Constant(true)("end", actual)

    } catch {
      case t: java.util.NoSuchElementException => () // catches the nullpointer exception if goals are not set
    }

    // done if "end" flag is set
    if (Apply(actual.get[Boolean]("end"), (s: Boolean) => s == true).generateValue()) return actual

    val xpos = Apply(actual.get[Int]("xpos"), (s: Int) => Constant(s)).generateValue()
    val ypos = Apply(actual.get[Int]("ypos"), (s: Int) => Constant(s)).generateValue()

    val edges = graph.getVertice(xpos, ypos).getEdges()

    if (edges.length == 0) { // checks if no outgoing edges are present
      Constant(true)("end", actual)
      return actual
    }
    Apply(actual.get[Boolean]("end"), (s: Boolean) => s)("end", next)
    Apply(actual.get[Int]("turns"), (s: Int) => s + 1)("turns", next)
    val costs = actual.get[Int]("costs")

    val nextEdge: Edge =
      FLAG match {
        case 0 => edges(Apply(discrete.Uniform((0 to edges.length - 1): _*), (i: Int) => i).generateValue())
        case 1 => {
          val edgesMinusLast = if (edges.length == 1 || lastEdge == null) edges else edges.filterNot { x => x.equals(lastEdge) }
          val costsInv = edgesMinusLast.map { x => 1 / x.getCost().generateValue().toDouble }
          var f: List[(Double, Edge)] = Nil
          for (i <- 0 to edgesMinusLast.length - 1) f = (costsInv(i), edgesMinusLast(i)) :: f
          lastEdge = Apply(Select(f: _*), (e: Edge) => e).generateValue()
          lastEdge
        }
        case 2 => {
          val costsInv = edges.map { x => 1 / x.getCost().generateValue().toDouble }
          var f: List[(Double, Edge)] = Nil
          for (i <- 0 to edges.length - 1) f = (costsInv(i), edges(i)) :: f
          Apply(Select(f: _*), (e: Edge) => e).generateValue()
        }
        case 3 => {
          val costsInv = edges.map {
            x =>
              if (distance(actual.get[Int]("xpos").generateValue(), actual.get[Int]("goalx").generateValue(), actual.get[Int]("ypos").generateValue(), actual.get[Int]("goaly").generateValue()) >
                distance(x.getVertice().getXPos().generateValue(), actual.get[Int]("goalx").generateValue(), x.getVertice().getYPos().generateValue(), actual.get[Int]("goaly").generateValue())) 0.9 else 0.1
          }
          var f: List[(Double, Edge)] = Nil
          for (i <- 0 to edges.length - 1) f = (costsInv(i), edges(i)) :: f
          lastEdge = Apply(Select(f: _*), (e: Edge) => e).generateValue()
          lastEdge
        }
        case 4 => {
          val costsInv = edges.map {
            x =>
              if (distance(actual.get[Int]("xpos").generateValue(), actual.get[Int]("goalx").generateValue(), actual.get[Int]("ypos").generateValue(), actual.get[Int]("goaly").generateValue()) >
                distance(x.getVertice().getXPos().generateValue(), actual.get[Int]("goalx").generateValue(), x.getVertice().getYPos().generateValue(), actual.get[Int]("goaly").generateValue()))
                0.9 * 1 / x.getCost().generateValue().toDouble else 0.1 * 1 / x.getCost().generateValue().toDouble
          }
          var f: List[(Double, Edge)] = Nil
          for (i <- 0 to edges.length - 1) f = (costsInv(i), edges(i)) :: f
          lastEdge = Apply(Select(f: _*), (e: Edge) => e).generateValue()
          lastEdge
        }
      }

    val nextVertice = nextEdge.getVertice()
    val cost = nextEdge.getCost()
    val c = Apply(nextVertice.getXPos(), (s: Int) => s)
    Constant(c.generateValue())("xpos", next)
    val d = Apply(nextVertice.getYPos(), (s: Int) => s)
    Constant(d.generateValue())("ypos", next)

    Apply(actual.get[Int]("costs"), cost, (s: Int, s2: Int) => s + s2)("costs", next)
    next
  }

  def printState(actual: Universe) {
    val x = Apply(actual.get[Int]("xpos"), (s: Int) => s).generateValue()
    val y = Apply(actual.get[Int]("ypos"), (s: Int) => s).generateValue()
    val c = Apply(actual.get[Int]("costs"), (s: Int) => s).generateValue()
    val e = Apply(actual.get[Boolean]("end"), (s: Boolean) => s).generateValue()
    println("X-Position: " + x + "\tY-Position: " + y + "\tCosts: " + c + "\tEnd: " + e)
  }

  def printUniverse(actual: Universe) {
    val x = Apply(actual.get[Int]("xpos"), (s: Int) => s).generateValue()
    val y = Apply(actual.get[Int]("ypos"), (s: Int) => s).generateValue()
    val gx = Apply(actual.get[Int]("goalx"), (s: Int) => s).generateValue()
    val gy = Apply(actual.get[Int]("goaly"), (s: Int) => s).generateValue()
    val c = Apply(actual.get[Int]("costs"), (s: Int) => s).generateValue()
    val e = Apply(actual.get[Boolean]("end"), (s: Boolean) => s).generateValue()
    println("X-Position: " + x + "\tY-Position: " + y + "\tX-Goal: " + gx + "\tY-Goal: " + gy + "\tCosts: " + c + "\tEnd: " + e)
  }

  def initializeUniverse(): Universe = {
    // create an initial universe
    val initNew = Universe.createNew()
    // start at Ludwigsburg
    Constant(10)("xpos", initNew)
    Constant(3)("ypos", initNew)
    Constant(0)("costs", initNew)
    Constant(0)("turns", initNew)
    Constant(false)("end", initNew)
    // destination is Berlin
    Constant(30)("goalx", initNew)
    Constant(30)("goaly", initNew)
    initNew
  }
  
    def initializeUniverse2(): Universe = {
    // create an initial universe
    val initNew = Universe.createNew()
    Constant(0)("xpos", initNew)
    Constant(0)("ypos", initNew)
    Constant(0)("costs", initNew)
    Constant(0)("turns", initNew)
    Constant(false)("end", initNew)
    Constant(ROW-1)("goalx", initNew)
    Constant(COL-1)("goaly", initNew)
    initNew
  }

  def travelingTo(): Element[(Int, Int)] = {
    var actual = initializeUniverse()
    for (i <- 0 until maxIterations) {
      actual = nextUniverse(actual)
      //      printState(actual)
      if (actual.get[Boolean]("end").generateValue()) return ^^(actual.get[Int]("costs"), actual.get[Int]("turns"))
    }
    ^^(Constant(-1), Constant(maxIterations))
  }
  
  def travelingToB(): Element[(Int, Int)] = {
    var actual = initializeUniverse2()
    for (i <- 0 until maxIterations) {
      actual = nextUniverse(actual)
//            printState(actual)
      if (actual.get[Boolean]("end").generateValue()) return ^^(actual.get[Int]("costs"), actual.get[Int]("turns"))
    }
    ^^(Constant(-1), Constant(maxIterations))
  }

  def travelingFromTo(fromX: Int, fromY: Int, toX: Int, toY: Int): Element[(Int, Int)] = {
    var actual = Universe.createNew()
    // start at Ludwigsburg
    Constant(fromX)("xpos", actual)
    Constant(fromY)("ypos", actual)
    Constant(0)("costs", actual)
    Constant(0)("turns", actual)
    Constant(false)("end", actual)
    Constant(toX)("goalx", actual)
    Constant(toY)("goaly", actual)
    printUniverse(actual)
    for (i <- 0 until maxIterations) {
      actual = nextUniverse(actual)
      printState(actual)
      if (actual.get[Boolean]("end").generateValue()) return ^^(actual.get[Int]("costs"), actual.get[Int]("turns"))
    }
    ^^(Constant(-1), Constant(maxIterations))
  }

  def distance(x1: Int, x2: Int, y1: Int, y2: Int) = {
    val x = Math.pow(x1 - x2, 2)
    val y = Math.pow(y1 - y2, 2)
    Math.sqrt(x + y)
  }

  def generateSample(size: Int, function: () => Element[(Int, Int)]) = {
    var list: List[Element[(Int, Int)]] = Nil
    for (i <- 0 to size) list = function() :: list
    list
  }

  def mean(list: List[Int]) = {
    val a = 0
    list.foldRight(a)((a: Int, b: Int) => a + b).toDouble / list.length
  }

  def process(samples: List[Element[(Int, Int)]], sampleSize: Int) {
    val costs = samples.map((i: Element[(Int, Int)]) => Apply(i, (s: (Int, Int)) => s._1).generateValue())
    val turns = samples.map((i: Element[(Int, Int)]) => Apply(i, (s: (Int, Int)) => s._2).generateValue())
    println("Average costs: " + mean(costs))
    println("Average turns: " + mean(turns))
    println("Probability to reach below a cost of "+TRESHOLD+": " + costs.count { x => x < TRESHOLD }.toDouble / sampleSize)
    println("Probability to reach in minimal cost("+MIN+"): " + costs.count { x => x == MIN }.toDouble / sampleSize)
  }

  //  def isEven(x:Int):Boolean={
  //    if(x%2==0) true
  //    else false
  //  }
  //  
  //  def isMinRow(actual:Universe):Element[Boolean]={
  //      Apply(actual.get[Int]("xpos"), (s: (Int)) => s==0)
  //   }
  //  
  //    def isMaxRow(actual:Universe):Element[Boolean]={
  //      Apply(actual.get[Int]("xpos"), (s: (Int)) => s==WIDTH-1)
  //   }
  //    
  //   def isMinCol(actual:Universe):Element[Boolean]={
  //      Apply(actual.get[Int]("ypos"), (s: (Int)) => s==0)
  //   }
  //  
  //  def isMaxCol(actual:Universe):Element[Boolean]={
  //      Apply(actual.get[Int]("ypos"), (s: (Int)) => s==HEIGHT-1)
  //  }
  def setDestination(x: Int, y: Int, universe: Universe): Universe = {
    Constant(x)("goalX", universe)
    Constant(y)("goalY", universe)
    universe
  }
  //  def generateNumericBoad() {
  //    for (i <- (0 to WIDTH - 1)) {
  //      for (j <- (0 to HEIGHT - 1)) {
  //        val temp = Apply(board(i)(j), (t: Int) => t)
  //        numericBoard(i)(j) = temp.generateValue();
  //      }
  //    }
  //  }
  //
  //  def printBoard() {
  //    for (i <- (0 to WIDTH-1)) {
  //      for (j <- (0 to HEIGHT-1)) {
  ////        if (j == -1 || i == -1 || j == HEIGHT || i == WIDTH)
  ////          print("\t")
  ////        else {
  //          val temp = Apply(board(i)(j), (t: Int) => t).generateValue()
  //          temp match {
  //            case -2 =>  print("\t")
  //            case -1 =>  print("# \t")
  //            case _ => print(temp + "\t")
  //          }
  ////        }
  //      }
  //      println()
  //    }
  //  }
  //  def printNumericBoard() {
  //    for (i <- (0 to WIDTH-1)) {
  //      for (j <- (0 to HEIGHT-1)) {
  ////                if (j == -1 || i == -1 || j == HEIGHT || i == WIDTH)
  ////          print("#\t")
  ////        else 
  //           numericBoard(i)(j) match {
  //            case -2 =>  print("\t")
  //            case -1 =>  print("# \t")
  //            case _ => print(numericBoard(i)(j) + "\t")
  //          }
  //      }
  //      println()
  //    }
  //  }
}