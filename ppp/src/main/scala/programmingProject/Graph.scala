package programmingProject

import com.cra.figaro.language.Constant
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.atomic._
import java.util.HashMap
import com.cra.figaro.language.Apply

class Graph() {

  var graph = new HashMap[Int, HashMap[Int, Vertice]]()

  def addVertice(vertice: Vertice) {
    val edges = vertice.getEdges()
    try {
      graph.get(vertice.getXPos().generateValue()).isEmpty() //.put(vertice.getYPos(),vertice)
    } catch {
      case t: NullPointerException => graph.put(vertice.getXPos().generateValue(), new HashMap[Int, Vertice]()) 
    } finally{
      graph.get(vertice.getXPos().generateValue()).put(vertice.getYPos().generateValue(), vertice)
    }
  }


  def getVertice(x: Int, y: Int) = graph.get(x).get(y)
  def getVertice(x: Constant[Int], y: Constant[Int]) = graph.get(x.generateValue()).get(y.generateValue())

  /*
           					Hamburg	 	-				29		-			x
            					|	15												|
            29 -		Hannover  -		29	-		-  	 Berlin
            |					|												/	  | 19
          Cologne   	35							 - 44 - 	Dresden
            |					|								|						|
            19 	-  Frankfurt	-	22 -	Nürnberg	- 32	
                			|	20					   | 17
                		Stuttgart -	23 - Munich
                                     
   */
    
  def generateAutobahn() {
    val Hamburg = new Vertice(Constant(10), Constant(40))
    val Hannover = new Vertice(Constant(10), Constant(30))
    val Berlin = new Vertice(Constant(30), Constant(30))
    val Dresden = new Vertice(Constant(30), Constant(20))
    val Cologne = new Vertice(Constant(0), Constant(20))
    val Frankfurt = new Vertice(Constant(10), Constant(10))
    val Nürnberg = new Vertice(Constant(20), Constant(10))
    val Stuttgart = new Vertice(Constant(10), Constant(0))
    val Munich = new Vertice(Constant(20), Constant(0))
    val Ludwigsburg = new Vertice(Constant(10), Constant(3))

    Hamburg.addEdge(new Edge(Hannover, Constant(15)))
    Hannover.addEdge(new Edge(Hamburg, Constant(15)))
    Hamburg.addEdge(new Edge(Berlin, Constant(29)))
    Berlin.addEdge(new Edge(Hamburg, Constant(29)))
    Hannover.addEdge(new Edge(Berlin, Constant(29)))
    Berlin.addEdge(new Edge(Hannover, Constant(29)))
    Hannover.addEdge(new Edge(Cologne, Constant(29)))
    Cologne.addEdge(new Edge(Hannover, Constant(29)))
    Hannover.addEdge(new Edge(Frankfurt, Constant(35)))
    Frankfurt.addEdge(new Edge(Hannover, Constant(35)))
    Frankfurt.addEdge(new Edge(Cologne, Constant(19)))
    Cologne.addEdge(new Edge(Frankfurt, Constant(19)))
    Frankfurt.addEdge(new Edge(Stuttgart, Constant(20)))
    Stuttgart.addEdge(new Edge(Frankfurt, Constant(20)))
    Stuttgart.addEdge(new Edge(Munich, Constant(23)))
    Munich.addEdge(new Edge(Stuttgart, Constant(23)))
    Munich.addEdge(new Edge(Nürnberg, Constant(17)))
    Nürnberg.addEdge(new Edge(Munich, Constant(17)))
    Nürnberg.addEdge(new Edge(Frankfurt, Constant(22)))
    Frankfurt.addEdge(new Edge(Nürnberg, Constant(22)))
    Nürnberg.addEdge(new Edge(Dresden, Constant(32)))
    Dresden.addEdge(new Edge(Nürnberg, Constant(32)))
    Nürnberg.addEdge(new Edge(Berlin, Constant(44)))
    Berlin.addEdge(new Edge(Nürnberg, Constant(44)))
    Dresden.addEdge(new Edge(Berlin, Constant(19)))
    Berlin.addEdge(new Edge(Dresden, Constant(19)))

    Ludwigsburg.addEdge(new Edge(Stuttgart, Constant(2)))
//    Stuttgart.addEdge(new Edge(Ludwigsburg, Constant(2)))
    Ludwigsburg.addEdge(new Edge(Frankfurt, Constant(18)))
//    Frankfurt.addEdge(new Edge(Ludwigsburg, Constant(18)))

    addVertice(Hamburg)
    addVertice(Hannover)
    addVertice(Berlin)
    addVertice(Dresden)
    addVertice(Cologne)
    addVertice(Frankfurt)
    addVertice(Nürnberg)
    addVertice(Stuttgart)
    addVertice(Munich)
    addVertice(Ludwigsburg)

  }

  def generateRandomGraph(col:Int, row:Int){  
      // settings 
  val COL = col
  val ROW = row
  val costs = List.range(1, 100)
  var board = Array.tabulate(COL, ROW)((x, y) => (new Vertice(Constant(x),Constant(y))))
    for (x <- (0 to COL - 1)) {
      for (y <- (0 to ROW - 1)) {
        if(x != COL-1){
          board(x)(y).addEdge(new Edge(board(x+1)(y),Apply(discrete.Uniform(costs:_*), (s: Int) => Constant(s)).generateValue()))
        }
        if(y != ROW-1){
          board(x)(y).addEdge(new Edge(board(x)(y+1),Apply(discrete.Uniform(costs:_*), (s: Int) => Constant(s)).generateValue()))
        }
      }
    }
    for (x <- (0 to COL - 1)) {
      for (y <- (0 to ROW - 1)) {
        addVertice(board(x)(y))
      }
    }
  }
  //  graph +=  (1 -> )
  //  
  //      (x == 0, y == 0) match {
  //        case (true, true) => (graph(x)(y) = new Vertice(Constant(x), Constant(y)))
  //        case (true, false) => ({
  //          val temp = new Vertice(Constant(x), Constant(y))
  //          val weight = Apply(discrete.Uniform(list: _*), (s: (Int)) => Constant(s)).generateValue()
  //          graph(x)(y - 1).addEdge(new Edge(temp, weight))
  //          graph(x)(y) = temp
  //        })
  //        case (false, true) => ({
  //          val temp = new Vertice(Constant(x), Constant(y))
  //          val weight = Apply(discrete.Uniform(list: _*), (s: (Int)) => Constant(s)).generateValue()
  //          graph(x - 1)(y).addEdge(new Edge(temp, weight))
  //          graph(x)(y) = temp
  //        })
  //        case (false, false) => ({
  //          val temp = new Vertice(Constant(x), Constant(y))
  //          val weight = Apply(discrete.Uniform(list: _*), (s: (Int)) => Constant(s)).generateValue()
  //          graph(x - 1)(y).addEdge(new Edge(temp, weight))
  //          val weight2 = Apply(discrete.Uniform(list: _*), (s: (Int)) => Constant(s)).generateValue()
  //          graph(x)(y - 1).addEdge(new Edge(temp, weight2))
  //          graph(x)(y) = temp
  //        })
  //      }
  //    }
}