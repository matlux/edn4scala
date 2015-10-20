package net.cabworks.test.edn4scala

import java.util.UUID

import clojure.lang.{Keyword, Symbol}
import net.cabworks.edn4scala.GistReader.{EdnKeyword, EdnSymbol}
import org.scalatest.FunSuite
import net.cabworks.edn4scala.GistReader

import scala.collection.immutable.HashMap

/**
 * Created by cab on 09/10/2015.
 */
class GistTest extends FunSuite {
  test("Read Edn") {
    assertResult(List(1, 2, 3)) {
      GistReader.readEdnString("(1 2 3)")
    }
  }

  def testEval (str : String) = GistReader.readEdnString(str)

  test("integers") {
    assertResult(0){testEval("0")}
    assertResult(1){testEval("0001")}
    assertResult(23) {testEval("23")}
    assertResult(1) {testEval("1")}
    assertResult(-43) { testEval("-43")}
  }
  test("floating point") {
    assertResult(53.1) {testEval("53.1")}
    assertResult(1.5) {testEval("1.5")}
    assertResult(0.0005) {testEval("0.0005")}
    assertResult(0.5) {testEval("0.5000")}
    assertResult(-3.14) {testEval("-3.14")}
  }

  test("equality") {
    assertResult(true) {
      val i = testEval("2")
      val d = testEval("2.0000000000000")
      i == d
    }

    assertResult(false) {
      val i = testEval("2")
      val d = testEval("2.0000000000001")
      i == d
    }
  }

  //  test("discard element") {
  //    assertResult(Vector(1,2,3)) { testEval("[1 2 #_discardMe 3]")}
  //  }

  test("nil ") {
    assertResult(null) { testEval("nil")}
  }

  test("booleans") {
    assertResult(true) { testEval("true")}
    assertResult(false) { testEval("false")}
  }

  test("string") {
    assertResult("1") { testEval("\"1\"")}
    assertResult("AA") { testEval("\"AA\"")}
    assertResult("") { testEval("\"\"")}
  }

  test("ratios") {
    assertResult(0.5) { testEval("1/2")}
    assertResult(0.5) { testEval("6/12")}
    assertResult(Set(0.5, 1)) { testEval("#{1/2 42/42}") }
  }


  test("#uuid tagged elem") {
    var uuid = UUID.randomUUID()
    assertResult(uuid) { testEval(s"""#uuid \"${uuid.toString}\"""") }
    assertResult(UUID.fromString("f81d4fae-7dec-11d0-a765-00a0c91e6bf6")) { testEval("#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\"")}
  }

  test("List") {
    assertResult(List()){testEval("()")}
    assertResult(List(1,2,3)) {testEval("(1 2 3)")}
    assertResult(List(1,2,3)) {testEval("(1,2,3)")}
  }

  test("vectors ") {
    assertResult(Vector(1)) { testEval("[1]")}
    assertResult(Vector(1,2, 3)) { testEval("[1 2 3]")}
    assertResult(Vector()) { testEval("[]")}
    assertResult(Vector(1,2,3)) {testEval("[1,2,3]")}
  }

  test("maps") {
    assertResult(Map(1 -> 12))(testEval("{ 1 12}") )

    assertResult(Map(1 -> 12, 2 -> 231))( testEval("{ 1 12 2 231}"))
    assertResult(Map())( testEval("{}"))
  }

  test("sets") {
    assertResult(Set(1, 2, 3))(testEval("#{1 2 3}") )

    assertResult(Set())( testEval("#{}"))
  }


  test("clojure keywords and symbols") {
    assertResult(EdnSymbol("a")) { testEval("a") }
    assertResult(EdnSymbol("f")) { testEval("f") }
    assertResult(EdnSymbol("test-namespace/state")) { testEval("test-namespace/state") }

    assertResult(EdnKeyword("a")) { testEval(":a") }
    assertResult(EdnKeyword("clojure.core","f")) { testEval("::f") }  //should this evaluate in clojure.core  ? in what other ns though ?
    assertResult(EdnKeyword("test", "asd")) { testEval(":test/asd") }

    assertResult(List("a", "b", "c", "d").map(EdnKeyword.apply)) { testEval("(:a :b :c :d )")}

    assertResult(List("a", "b", "c", "d").map(EdnSymbol.apply)) { testEval("(a b c d )")}
    assertResult(Vector("a", "b", "c", "d").map(EdnSymbol.apply)) { testEval("[a b c d ]")}
  }
}
