package net.cabworks.edn4scala



import scala.io.StdIn

/**
 * Created by cab on 12/10/2015.
 */
object EdnRepl {

  def |>[T, R] (a : T, f : (T) => R) : R = f(a)

  def replLoop : Unit = {
    val input = StdIn.readLine("Edn>")
    input match {
      case in : String if in.length() > 0 => {
        //this |> ( EdnParser.eval(input), println)
       // val result = EdnParser.read(in)
        //val result = Evaluator.evalString(in)
        println(input)
        replLoop
      }
      case _ => ()
    }

  }

  def main(args: Array[String]) = replLoop
}
