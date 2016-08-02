package programmingProject

import com.cra.figaro.library.collection._
import com.cra.figaro.language._

class Vertice(x: Constant[Int], y: Constant[Int]) {
  private val xpos = x
  private val ypos = y
  private var edges = Seq.empty[Edge]
  def getXPos() = xpos
  def getYPos() = ypos
  def addEdge(edge: Edge) {edges = edges :+ edge}
  def getEdges() = edges
  def setEdges(e: Seq[Edge]) { edges = e}
  def getCosts() = Container(edges.map { x => x.getCost() }:_*)
}

