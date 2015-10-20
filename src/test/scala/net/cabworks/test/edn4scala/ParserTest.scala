package net.cabworks.test.edn4scala


import net.cabworks.edn4scala.GistReader.{EdnKeyword, EdnSymbol}
import org.scalatest.FunSuite
import net.cabworks.edn4scala.{GistReader}
import java.util.UUID
/**
 * Created by cab on 03/10/2015.
 */

class ParserTest extends FunSuite{
  def testParser (str : String) = GistReader.readEdnString(str)

  test("integers") {
    assertResult(0){testParser("0")}
    assertResult(1){testParser("0001")}
    assertResult(23) {testParser("23")}
    assertResult(1) {testParser("1")}
    assertResult(-43) { testParser("-43")}
  }
  test("floating point") {
    assertResult(53.1) {testParser("53.1")}
    assertResult(1.5) {testParser("1.5")}
    assertResult(0.0005) {testParser("0.0005")}
    assertResult(0.5) {testParser("0.5000")}
    assertResult(-3.14) {testParser("-3.14")}
  }

  test("equality") {
    assertResult(true) {
      val i = testParser("2")
      val d = testParser("2.0000000000000")
      i == d
    }

    assertResult(false) {
      val i = testParser("2")
      val d = testParser("2.0000000000001")
      i == d
    }
  }

//  test("discard element") {
//    assertResult(Vector(1,2,3)) { testEval("[1 2 #_discardMe 3]")}
//  }

  test("nil ") {
    assertResult(null) { testParser("nil")}
  }

  test("booleans") {
    assertResult(true) { testParser("true")}
    assertResult(false) { testParser("false")}
  }
  
  test("string") {
    assertResult("1") { testParser("\"1\"")}
    assertResult("AA") { testParser("\"AA\"")}
    assertResult("") { testParser("\"\"")}
  }

  test("ratios") {
    assertResult(0.5) { testParser("1/2")}
    assertResult(0.5) { testParser("6/12")}
    assertResult(Set(0.5, 1)) { testParser("#{1/2 42/42}") }
  }

  test("#uuid tagged elem") {
    var uuid = UUID.randomUUID()
    assertResult(uuid) { testParser(s"""#uuid \"${uuid.toString}\"""") }
    assertResult(UUID.fromString("f81d4fae-7dec-11d0-a765-00a0c91e6bf6")) { testParser("#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\"")}
  }

  test("List") {
    assertResult(List()){testParser("()")}
    assertResult(List(1,2,3)) {testParser("(1 2 3)")}
    assertResult(List(1,2,3)) {testParser("(1,2,3)")}
  }

  test("vectors ") {
    assertResult(Vector(1)) { testParser("[1]")}
    assertResult(Vector(1,2, 3)) { testParser("[1 2 3]")}
    assertResult(Vector()) { testParser("[]")}
    assertResult(Vector(1,2,3)) {testParser("[1,2,3]")}
  }

  test("maps") {
    assertResult(Map(1 -> 12))(testParser("{ 1 12}") )

    assertResult(Map(1 -> 12, 2 -> 231))( testParser("{ 1 12 2 231}"))
    assertResult(Map())( testParser("{}"))
  }

  test("sets") {
    assertResult(Set(1, 2, 3))(testParser("#{1 2 3}") )

    assertResult(Set())( testParser("#{}"))
  }


  test("clojure keywords and symbols") {
    assertResult(EdnSymbol("a")) { testParser("a") }
    assertResult(EdnSymbol("f")) { testParser("f") }
    assertResult(EdnSymbol("test-namespace/state")) { testParser("test-namespace/state") }

    assertResult(EdnKeyword("a")) { testParser(":a") }
    //assertResult(EdnKeyword(":f")) { testParser("::f") }
    assertResult(EdnKeyword("test/asd")) { testParser(":test/asd") }

    assertResult(List("a", "b", "c", "d").map(EdnKeyword(_))) { testParser("(:a :b :c :d )")}

    assertResult(List("a", "b", "c", "d").map(EdnSymbol(_))) { testParser("(a b c d )")}
    assertResult(Vector("a", "b", "c", "d").map(EdnSymbol(_))) { testParser("[a b c d ]")}
  }

  test("arithmetic") {
    assertResult(List(EdnSymbol("+"), 2, 2)) {
      testParser("(+ 2 2)")
    }
    assertResult(List(EdnSymbol("-"), 2, 2)) {
      testParser("(- 2 2)")
    }
    assertResult(List(EdnSymbol("/"), 2, 2)) {
      testParser("(/ 2 2)")
    }
    assertResult(List(EdnSymbol("*"), 2, 2)) {
      testParser("(* 2 2)")
    }
  }

  test("quoted lists") {
    assertResult(List(EdnSymbol("quote"), List(1, 2, 2))) {
      testParser("'(1 2 2)")
    }
    assertResult(List(EdnSymbol("quote"), List(EdnSymbol("+"), 2, 2))) {
      testParser("'(+ 2 2)")
    }

  }

  test("defs") {
    assertResult(List(EdnSymbol("def"), EdnSymbol("a"), 1)) {
      testParser("(def a 1)")
    }
  }

  test("fns") {
    assertResult(List(EdnSymbol("fn"), Vector(EdnSymbol("a"), EdnSymbol("b")), List(EdnSymbol("+"), EdnSymbol("a"), EdnSymbol("b")))) {

      testParser("(fn [a b] (+ a b))")
    }
  }
}
