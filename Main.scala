import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.matching.Regex

abstract class Tree

case class Or(l: Tree, r: Tree) extends Tree
case class And(l: Tree, r: Tree) extends Tree
case class Not(c: Tree) extends Tree
case class Parenth(c: Tree) extends Tree
case class Bool(c: String) extends Tree
case class Var(c: String) extends Tree

object Main {

  def printTree(t: Tree): String = t match {
    case Or(l,r) =>
      printTree(l) + " || " + printTree(r)
    case And(l,r) =>
      printTree(l) + " && " + printTree(r)
    case Not(c) =>
      "!" + printTree(c)
    case Parenth(c) =>
      "(" + printTree(c) + ")"
    case Bool(c) =>
      c.toString
    case Var(c) =>
      c.toString
  }

  def simplify(t: Tree): Tree = t match{
    case Or(l,r) =>
      simplifyOr(Or(simplify(l),simplify(r)))
    case And(l,r) =>
      simplifyAnd(And(simplify(l),simplify(r)))
    case Not(c) =>
      simplifyNot(Not(simplify(c)))
    case Parenth(c) =>
      simplifyParenth(Parenth(simplify(c)))
    case Var(c) =>
      Var(c)
    case Bool(c) =>
      Bool(c)
  }

  def simplifyOr(t: Tree): Tree = t match {
    case Or(Bool(x),r) if x == "true" =>
      Bool("true")
    case Or(l,Bool(y)) if y == "true" =>
      Bool("true")

    case Or(Bool(x),r) if x == "false" =>
      simplify(r)
    case Or(l,Bool(y)) if y == "false" =>
      simplify(l)

    case Or(Var(x), Var(y)) if x == y =>
      Var(x)

    case Or(Var(x),Var(y)) if ("!" + x) == y =>
      Bool("true")
    case Or(Var(x),Var(y)) if ("!" + y) == x =>
      Bool("true")

    case Or(l,r) =>
      Or(l,r)
  }

  def simplifyAnd(t: Tree): Tree = t match {
    case And(Bool(x),r) if x == "false" =>
      Bool("false")
    case And(l,Bool(y)) if y == "false" =>
      Bool("false")

    case And(Bool(x),r) if x == "true" =>
      simplify(r)
    case And(l,Bool(y)) if y == "true" =>
      simplify(l)

    case And(Var(x),Var(y)) if x == y =>
      Var(x)

    case And(Var(x),Var(y)) if "!" + x == y =>
      Bool("false")
    case And(Var(x),Var(y)) if "!" + y == x =>
      Bool("false")

    case And(l,r) =>
      And(l,r)
  }

  def simplifyNot(t: Tree): Tree = t match {
    case Not(Bool(x)) if x == "true" =>
      Bool("false")
    case Not(Bool(x)) if x == "false" =>
      Bool("true")

    case Not(Var(c)) =>
      Var("!" + c)

    case Not(c) =>
      Not(c)
  }

  def simplifyParenth(t: Tree): Tree = t match {
    case Parenth(Var(c)) =>
      Var(c)
    case Parenth(Bool(c)) =>
      Bool(c)
    case Parenth(c) =>
      Parenth(c)
  }

  def printSimplified(t: Tree): String = t match {
    case Or(l,r) =>
      printSimplified(l) + " || " + printSimplified(r)

    case And(l,r) =>
      printSimplified(l) + " && " + printSimplified(r)

    case Not(c) =>
      "!" + printSimplified(c)

    case Parenth(Var(c)) =>
      c
    case Parenth(Bool(c)) =>
      c
    case Parenth(c) =>
      "(" + printSimplified(c) + ")"

    case Var(c) =>
      c

    case Bool(c) =>
      c
  }

  def main(args: Array[String]): Unit = {
    while (true) {
      print("expression? ")
      val input = readLine()
      //println(input.charAt(4))
      val Length = input.length()
      val rd = new RecursiveDescent(Length)
      val exp: Tree = rd.parseE(input)
      //println(exp)
      print("input: ")
      println(printTree(exp))
      val simplifiedExp: Tree = simplify(exp)
      print("result: ")
      println(printSimplified(simplifiedExp))
      println()
    }
  }
}

//struct to hold indices of sets of parenthesis
class Parenthesis(l: Int, r: Int) {
  val left: Int = l
  val right: Int = r
}

//parse input into Tree
class RecursiveDescent(Length: Int) {
  val varregex: Regex = "^[A-Za-z]+".r

  //finds all sets of parenthesis in an input string and Parenthesis objects to input list
  def findParenthesis(in: String, list: ListBuffer[Parenthesis]): Unit = {
    var offset = 0
    var arrBufferL = new ArrayBuffer[Int]()
    var indexL = in.indexOf("(")
    while(offset < in.length && indexL >= 0){
      arrBufferL.append(indexL)
      offset = indexL
      indexL = in.indexOf("(", offset + 1)
    }

    if(arrBufferL.isEmpty){
      return
    }

    offset = 0
    var arrBufferR = new ArrayBuffer[Int]()
    var indexR = in.indexOf(")")
    while(offset < in.length && indexR >= 0){
      arrBufferR.append(indexR)
      offset = indexR
      indexR = in.indexOf(")", offset + 1)
    }

    while(arrBufferL.nonEmpty){
      offset = -1
      var length = arrBufferL.length
      if(length == 1){
        list.append(new Parenthesis(arrBufferL(0), arrBufferR(0)))
        arrBufferL.remove(0)
        arrBufferR.remove(0)
      }
      else{
        for( i <- 0 until length) {
          if (arrBufferL(i) < arrBufferR(0)) {
            offset += 1
          }
        }

        list.append(new Parenthesis(arrBufferL(offset), arrBufferR(0)))
        arrBufferL.remove(offset)
        arrBufferR.remove(0)
      }
    }
  }

  //checks if the ref index is inside any of the sets of parenthesis inside list
  def insideCheck(list: List[Parenthesis], ref: Int): Boolean = {
    //checks if reference index is inside and parenthesis
    //return true if inside, false if outside
    for (p <- list) {
      if ((ref > p.left) && (ref < p.right)) {
        return true
      }
    }
    false
  }

  //1) finds parenthesis in input string
  //2) then finds first ||
  //3) checks if index of || is inside of parenthesis
  //4) if so find the next || and check
  //5) repeat until no more || or index is not inside parenthesis
  //6) if a valid || index is found (meaning not inside of any parenthesis), E := T '||' E
  //7) else, E := T
  def parseE(str: String): Tree = {
    //1
    val listBuffer = new ListBuffer[Parenthesis]
    findParenthesis(str, listBuffer)
    val list = listBuffer.toList
    var index = 0
    //2
    var ref = str.indexOf("||")
    var check = true
    //5
    while ((ref >= 0) && check) {
      //3
      if (insideCheck(list, ref)) {
        index = ref + 2
        //4
        ref = str.indexOf("||", index)
      }
      else {
        check = false
      }
    }
    //6
    if (ref >= 0) {
      val start = ref - 1
      val end = ref + 3
      val startStr = str.substring(0, start)
      val endStr = str.substring(end)
      Or(parseT(startStr), parseE(endStr))
    }
    //7
    else {
      parseT(str)
    }
  }

  //1) finds parenthesis in input string
  //2) then finds first &&
  //3) checks if index of && is inside of parenthesis
  //4) if so find the next && and check
  //5) repeat until no more && or index is not inside parenthesis
  //6) if a valid && index is found, T := F '&&' T
  //7) else, T := F
  def parseT(str: String): Tree = {
    //1
    val listBuffer = new ListBuffer[Parenthesis]
    findParenthesis(str, listBuffer)
    val list = listBuffer.toList
    var index = 0
    //2
    var ref = str.indexOf("&&")
    var check = true
    //5
    while ((ref >= 0) && check) {
      //3
      if (insideCheck(list, ref)) {
        index = ref + 2
        //4
        ref = str.indexOf("&&", index)
      }
      else {
        check = false
      }
    }
    //6
    if (ref >= 0) {
      val start = ref - 1
      val end = ref + 3
      val startStr = str.substring(0, start)
      val endStr = str.substring(end)
      And(parseF(startStr), parseT(endStr))
    }
    //7
    else {
      parseF(str)
    }
  }

  //1) if first char in input string is ! then, F := '!' A
  //2) else, F := A
  def parseF(str: String): Tree = {
    //1
    if (str.charAt(0).equals('!')) {
      val startStr = str.substring(1)
      Not(parseA(startStr))
    }
    //2
    else {
      parseA(str)
    }
  }

  //1) if first char in input string is ( then find ) and, A := '(' E ')'
  //2) else, A := C
  def parseA(str: String): Tree = {
    //1
    val list = new ListBuffer[Parenthesis]
    findParenthesis(str, list)
    if (str.charAt(0).equals('(')) {
      var end = 0
      list.foreach(p => if(p.right > end){
        end = p.right
      })
      val startStr = str.substring(1, end)
      Parenth(parseE(startStr))
    }
    //2
    else {
      parseC(str)
    }
  }

  //1) check if "true", C := "true"
  //2) if "false", C := "false"
  //3) else, C := c
  def parseC(str: String): Tree = {
    //1
    if (str.contains("true")) {
      Bool("true")
    }
    //2
    else if (str.contains("false")) {
      Bool("false")
    }
    //3
    else {
      parseV(str)
    }
  }

  //1) check string is a single char
  //2) check string is an alphabetical character
  //3) c := char
  def parseV(str: String): Tree = {
    //1
    if(str.length() != 1){
      throw new RuntimeException("Incorrect variable syntax: variable cannot be string")
    }
    //2
    val vars = varregex.findAllIn(str)
    if(!vars.hasNext){
      throw new RuntimeException("Incorrect variable syntax: not an alphabetical character")
    }
    val varname = vars.next()
    Var(varname.charAt(0).toString)
  }
}