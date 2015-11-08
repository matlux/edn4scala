package net.cabworks.test.edn4scala


import net.cabworks.edn4scala.EdnParser._
import org.scalatest.{Matchers, FreeSpec, FunSuite}
import net.cabworks.edn4scala.EdnParser
import java.util.UUID
/**
 * Created by cab on 03/10/2015.
 */

class EdnParserTest extends FreeSpec with Matchers {
  "Read Edn list" in {
    parse("(1 2 3)") should be(EdnList(List(1,2,3)))
  }

  "integers" in {
    parse("0") should be(0)
    parse("0001") should be(1)
    parse("32") should be(32)
    parse("-43") should be(-43)

  }

  "floating point" in {
    parse("53.1") should be(53.1)
    parse("1.5") should be(1.5)
    parse("0.0004") should be(0.0004)
    parse("-3.14") should be(-3.14)
  }

  "equality " in {
    val a = parse("2")
    val b = parse("2.0000000")
    val c = parse("2.00000001")

    a should be(b)
    a should not be c
    b should not be c

  }

  "nil to null" in {
    parse("nil") == null shouldBe true
  }

  "booleans" in {
    parse("true") shouldBe true
    parse("false") shouldBe false
  }

  "strings" in {
    parse("\"1\"") shouldBe "1"
    parse("\"AA\"") shouldBe "AA"
    parse("\"\"") shouldBe ""
  }

  "ratios" in {
    parse("1/2") shouldBe 0.5
    parse("6/12") shouldBe 0.5
    parse("#{1/2 42/42}") shouldBe EdnSet(Set(0.5, 1))
  }

  "#uuid tagged elements" in {
   val uuid = UUID.randomUUID()
    parse(s"""#uuid \"${uuid.toString}\"""") shouldBe uuid
  }

  "Lists" in {
    parse("()") shouldBe EdnList(List())
    parse("(1 2 3)") shouldBe EdnList(List(1, 2, 3))
  }

  "Vectors" in {
    parse("[]") shouldBe EdnVector(Vector())
    parse("[2,3]") shouldBe EdnVector(Vector(2,3))
  }

  "Maps" in {
    parse("{1 2 34 5}") shouldBe EdnMap(Map(1->2, 34->5))
    parse("{}") shouldBe EdnMap(Map())
  }

  "Sets" in {
    parse("#{}") shouldBe EdnSet(Set())
    parse("#{7 6 23}") shouldBe EdnSet(Set(7, 6, 23))
  }

  "symbols and keywords" in {
    parse("a") should be(EdnSymbol("a"))
    parse("f") should be(EdnSymbol("f"))
    parse("test-namespace/state") should be(EdnSymbol("test-namespace", "state"))


    parse(":a") should be(EdnKeyword("a"))
    parse(":test/a") should be(EdnKeyword("test","a"))

    parse("(:a :b :c :d )") shouldBe EdnList(List("a", "b", "c", "d").map(EdnKeyword(_)))
    parse("(a b c d )") shouldBe EdnList(List("a", "b", "c", "d").map(EdnSymbol(_)))
    parse("[:a :b :c :d ]") shouldBe EdnVector(Vector("a", "b", "c", "d").map(EdnKeyword(_)))
  }

  "arithmetic" in {
    parse("(+ 2 2)") shouldBe EdnList(List(EdnSymbol("+"), 2, 2))
    parse("(- 2 2)") shouldBe EdnList(List(EdnSymbol("-"), 2, 2))
    parse("(/ 2 2)") shouldBe EdnList(List(EdnSymbol("/"), 2, 2))
    parse("(* 2 2)") shouldBe EdnList(List(EdnSymbol("*"), 2, 2))
  }

  "quoted lists" in {
    parse("'(1 2 3)") shouldBe EdnList(List(EdnSymbol("quote"), EdnList(List(1, 2, 3))))
    parse("'(+ 2 2)") shouldBe EdnList(List(EdnSymbol("quote"), EdnList(List(EdnSymbol("+"), 2, 2))))
  }

  "defs" in {
    parse("(def a 1)") shouldBe EdnList(List(EdnSymbol("def"), EdnSymbol("a"), 1))
  }

  "fns" in {
    parse("(fn [a b] (+ a b))") shouldBe EdnList(List(EdnSymbol("fn"),
      EdnVector(Vector(EdnSymbol("a"), EdnSymbol("b"))),
      EdnList(List(EdnSymbol("+"), EdnSymbol("a"), EdnSymbol("b")))))
  }

  def parse(s: String) = EdnParser.readEdnString(s)
}

