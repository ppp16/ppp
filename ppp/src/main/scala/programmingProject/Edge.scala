package programmingProject

import com.cra.figaro.language._

class Edge(vertice: Vertice, cost: Constant[Int]) {
  private val next = vertice
  private val weight = cost
  
  def getVertice() = next
  def getCost() = weight
  
}