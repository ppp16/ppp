

object test {
  def main(args: Array[String]): Unit = {
    println(eval(test1))
    println(eval(test2))
  }

/**
Homework 01
============
Submit your solution to this exercise until Tuesday, 26.4., 23:59h
via email to paolo.giarrusso@uni-tuebingen.de.
Work in groups of 1 or 2 students.
 
 Put "pl1-hw01" in subject, please
Write in the email:
 - your names
 - your student ids ("Matrikelnummer")
 - your study programme ("Studiengang")
 - how long have you been studying ("Fachsemester
*/

/**
Consider the language of arithmetic expressions with "with", 
as illustrated by the following abstract syntax:
*/
sealed abstract class Exp 
case class Num(n: Int) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class Mul(lhs: Exp, rhs: Exp) extends Exp
case class Id(x: Symbol) extends Exp 
case class With(x: Symbol, xdef: Exp, body: Exp) extends Exp
 
/**
We use implicits again to make example programs less verbose. 
*/
implicit def num2exp(n: Int) = Num(n)
implicit def sym2exp(x: Symbol) = Id(x)

/**
Your task is to extend the language with the following new binding constructs:
*/

case class Let(defs: List[(Symbol, Exp)], body: Exp) extends Exp
case class LetStar(defs: List[(Symbol, Exp)], body: Exp) extends Exp

/**
The purpose of the Let construct is to bind a list of identifiers in such a way
that the scope of the bound variables is only in the body, but not any of the
right hand sides of definitions. In particular, there is no shadowing between the definitions. 
For instance, the following test case should evaluate to 7 and not to 11:
*/

val test1 = 
  With('x, 1,
   Let(List('x -> 5, 'y -> Add('x,1)),Add('x,'y)))

/**
The LetStar construct is similar to let, but the scope of a definition contains all
right hand sides of definitions that follow the current one.
The following test case should hence evaluate to 11.   
Note: The names "Let" and "LetStar" have been choosen in analogy to the
"let" and "let*" binding constructs in Scheme and Racket.
*/

val test2 = 
     With('x, 1,
      LetStar(List('x -> 5, 'y -> Add('x,1)),Add('x,'y)))

/**
Tasks:
      
      1) Implement the missing part of the eval and subst function
      to support Let and LetStar
      
      2) There is some redundancy in the binding constructs of this
      language. Try to eliminate at least one (or even two) of the
      language constructs With,Let,LetStar by defining the eliminated
      binding constructs as syntactic sugar.
      
      3) Bonus exercise (not mandatory): Implement a variant
      "Letr" of "Let" in which the scope of a definition includes
      all right-hand sides of definitions in the same block, including
      earlier definitions. For instance,
      Letr(List('x -> 'y, 'y -> 1),Add('x,'y)) 
      should evaluate to 2.
      On 
      Letr(List('x -> 'y, 'y -> 'x),Add('x,'y)) 
      the interpreter should loop or terminate with an error message.
*/      
   
def subst(e: Exp,i: Symbol,v : Num) : Exp = e match {
    case Num(n) => e
    case Id(x) => if (x == i) v else e
    case Add(l,r) => Add( subst(l,i,v), subst(r,i,v))
    case Mul(l,r) => Mul( subst(l,i,v), subst(r,i,v))
    case With(x,xdef,body) => With(x,
                                   subst(xdef,i,v),
                                   if (x == i) body else subst(body,i,v))
   case Let(defs, body) => Let(defs.map {case (sym, exp) => (sym, subst(exp, i, v))},
      if(defs.map(x => x._1).contains(i)) body else subst(body, i, v))
     
//     Let(defs.map {case (sym, exp) => (sym, subst(body, i, v))}, 
//       defs.find{case (sym, exp) => sym == i}.fold(subst(body, i, v))(_ => body))
       
//     Let(defs.map {case (sym, exp) => (sym, subst(exp, i, v))}, body)
//     With(defs.head._1,defs.head._2,
//                if (defs.tail == Nil)body else Let(defs.tail,body))    
                
   case LetStar(defs, body) => e // some sugar
   
//    case LetStar(defs, body) => {
//      val index = defs.indexWhere{case (sym, exp) => sym == i}
//      val (front, back) = defs.splitAt(index)
//      LetStar(front.map{case (sym, exp) => (sym, subst(exp, i, v))} ::: back , if(defs.map(x => x._1).contains(i)) body else subst(body, i, v))
//    }
   
//     LetStar(defs.tail.map{case (sym,exp) => (sym,subst(exp,defs.head._1,eval(defs.head._2)))},
//       subst(body,defs.head._1,eval(defs.head._2)))                             
}

def eval(e: Exp) : Int = e match {
  case Num(n) => n
  case Id(x) => sys.error("unbound variable: " + x.name)
  case Add(l,r) => eval(l) + eval(r)
  case Mul(l,r) => eval(l) * eval(r)
  case With(x, xdef, body) => eval(subst(body,x,Num(eval(xdef)))) 
  case Let(defs,body) =>  eval(defs.foldLeft(body){case (current, (sym, next)) => With(sym, next, current)})
//    defs match {
//      case Nil => eval(body)
//      case (sym, exp) :: tail => eval(Let(tail, subst(body, sym, eval(exp))))
//    }
    
//    defs match {
//      case Nil => eval(body)
//      case head :: tail => eval(Let(tail, subst(body, head._1, eval(head._2))))
//    }
  case LetStar(defs,body) => eval(defs.reverse.foldLeft(body){case (current, next) => Let(List(next), current)})
//    defs match {
//      case Nil => eval(body)
//      case head :: tail => eval(Let(tail.map{case (sym, exp) => (sym, With(head._1, head._2, exp))}, subst(body, head._1, eval(head._2))))
//    }
  
  
//    defs match {
//      case Nil => eval(body)
//      case head :: tail => eval(LetStar(tail, subst(body, head._1, eval(head._2))))
//    }
}

}