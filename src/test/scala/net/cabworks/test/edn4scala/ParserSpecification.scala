package net.cabworks.test.edn4scala

import net.cabworks.edn4scala.EdnParser
import net.cabworks.edn4scala.EdnParser._
import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll}
import org.scalacheck.Gen
/**
 * Created by cab on 10/11/2015.
 */
object ParserSpecification extends  Properties("EdnParser") {


  property("Lists") = forAll { (xs: List[Int]) => {
    val ednStr = s"(${xs.mkString(" ")})"
    EdnParser.readEdnString(ednStr) match {
      case EdnList(ys, _) => ys == xs
      case _ => false
    }
  }
  }


  property("Vectors") = forAll { (xs: Vector[Int]) => {
    val ednStr = s"[${xs.mkString(" ")}]"
    EdnParser.readEdnString(ednStr) match {
      case EdnVector(ys, _) => ys == xs
      case _ => false
    }
  }
  }



  property("Sets") = forAll { (xs: Set[Int]) => {
    val ednStr = s"#{${xs.mkString(" ")}}"
    EdnParser.readEdnString(ednStr) match {
      case EdnSet(ys, _) => ys == xs
      case _ => false
    }
  }
  }

  property("Maps") = forAll { (xs: Map[Int, Int]) => {
    val content = xs.map({ case (k, v) => s"$k $v" }).mkString(" ")
  //  println("<<<<<<<<<<<<<<" + content)
    val ednStr = s"{$content}"
    EdnParser.readEdnString(ednStr) match {
      case EdnMap(ys, _) => ys == xs
      case _ => false
    }
   }
  }

  val symbolNameGen = for { c <- Gen.nonEmptyListOf[Char](Gen.oneOf(Gen.alphaChar, Gen.alphaNumChar))} yield c

  property("Symbols") = forAll( Gen.identifier) { s =>
    EdnParser.readEdnString(s) match {
      case EdnSymbol(ns, s, _) => true
      case _ => false
    }
  }
}
