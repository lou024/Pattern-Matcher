import scala.util.matching.Regex

abstract class S
//S->E

case class E(left: T, right: Option[E2]) extends S
//E->T E2

case class E2(left: E3) extends S
//E2->'|' E3

case class E3(left: T, right: Option[E2]) extends S
//E3->T E2

case class T(left: F, right: Option[T2]) extends S
//T->F T2

case class T2(left: F, right: Option[T2]) extends S
//T2->F T2

case class F(left: A, right: Option[F2]) extends S
//F->A F2

case class F2(left: Option[F2]) extends S
//F2->'?' F2
//F2->NIL

abstract class A extends S
//A->'(' A2
//A->C
case class A2(left: E) extends A
//A2->E ')'

case class C(left: Char) extends A

class RecursiveDescent(input:String) {
  val parregex: Regex = "^[0-9]+".r
  val varregex: Regex = "^[A-Za-z]+".r

  var index = 0
  var expLength = 0

  def parseS(): S = parseE()
  //S->E

  def parseE(): E = E(parseT(), parseE2())
  //E->T E2

  def parseE2(): Option[E2] = {
    //E2-> '|' E3
    if(index < input.length && input(index) == '|'){
      index+=1
      Some(E2(parseE3()))
    }
    else None
  }

  def parseE3(): E3 = {
    //E3->T E2
    E3(parseT(), parseE2())
  }

  def parseT(): T = {
    //T->F T2
    T(parseF(), parseT2())
  }

  def parseT2() : Option[T2] = {
    //T2->F T2
    if(index < input.length && input(index) != ')'
      && input(index) != '?' && input(index) != '|'){
      Some(T2(parseF(), parseT2()))
    }
    else None
  }

  def parseF() : F = F(parseA(), parseF2())
  //F->A F2

  def parseF2() : Option[F2] = {
    //F2->F2
    //F2->None
    if(index < input.length && input.charAt(index) == '?'){
      index+=1
      Some(F2(parseF2()))
    }
    else None
  }

  def parseA() : A = {
    //A->'(' A2
    //A->C
    if(index < input.length && input.charAt(index) == '('){
      index+=1
      parseA2()
    } else parseC()
  }

  def parseA2() : A2 = {
    //A2->E ')'
    val a2 = A2(parseE())
    if(index < input.length && input.charAt(index) == ')') {
      index += 1
      a2
    } else throw new RuntimeException("bluep")
  }

  def parseC(): C = {
    val currStr = input.charAt(index)
    index+=1
    C(currStr)
  }

}

object Main {
  type Environment = String => Char
  var index = 0
  var optional:Boolean = false

  var insideParentheses:Boolean = false
  var parInd = 0


  def eval(tree: Any, str: String): Boolean = {
    tree match {
      case E(t, e2)=>{
        if(e2.isEmpty) return eval(t, str)
        val saveInd = index

        var result = eval(t, str)
        if(!result){
          index = saveInd
          result = eval(e2, str)
        }
        result
      }
      case None=>false
      case Some(t2:T2)=>{
        eval(t2, str)
      }
      case Some(f2: F2)=>{
        eval(f2, str)
      }
      case Some(e2: E2)=>{
        eval(e2, str)
      }
      case E2(e3)=>{
        eval(e3, str)
      }
      case E3(t, e2)=>{
        if(e2.isEmpty) return eval(t, str)
        val saveInd = index

        var result = eval(t, str)
        if(!result) {
          index = saveInd
          result = eval(e2, str)
        }
        result
      }
      case T(f, t2)=>{
        if(t2.isEmpty)return eval(f, str)
        eval(f, str) & eval(t2, str)
      }
      case T2(f, t2)=>{
        if(t2.isEmpty) return eval(f, str)
        eval(f, str) & eval(t2, str)
      }
      case F(a, f2)=>{
        if(f2.isEmpty){
          return eval(a, str)
        }
        val saveInd = index
        var result = eval(a, str)
        if (!result) {
          index = saveInd
          result = !result
        }
        result
      }
      case F2(f2)=>{
        eval(f2, str)
      }
      case A2(e)=>{
        eval(e, str)
      }
      case C(c)=>{
        if(index >= str.length)return false
        if(c == '.')return {
          index+=1
          true
        }
        val result = c == str(index)
        if(result)index+=1
        result
      }

    }
  }

  def main(args: Array[String]){

    var exp = scala.io.StdIn.readLine("Pattern? ")
    val rd = new RecursiveDescent(exp)
    val pattern:S = rd.parseE()

    while(true){
      exp = scala.io.StdIn.readLine("Match? ")
      if(exp == "q")System.exit(0)
      val temp: Boolean = eval(pattern, exp)
      if(temp && Main.index == exp.length){
        println("Match\n")
      }else {
        println("No Match\n" )
      }
      index = 0

    }

  }

}