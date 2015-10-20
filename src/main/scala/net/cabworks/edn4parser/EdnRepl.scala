package net.cabworks.edn4scala



import scala.io.StdIn
import scala.util.{Try, Failure, Success}

/**
 * Created by cab on 12/10/2015.
 */
object EdnRepl {


  def replLoop : Unit = {
    val input = StdIn.readLine("Edn> ")
    input match {
      case in : String if in.length() > 0 => {
        val result = Try(EdnParser.readEdnString(in)) match {
          case Success(res) => res
          case Failure(ex) => println(ex.getMessage)
        }
        println(EdnParser.writeEdnString(result))
        replLoop
      }
      case _ => ()
    }

  }

  def main(args: Array[String]) = replLoop
}
