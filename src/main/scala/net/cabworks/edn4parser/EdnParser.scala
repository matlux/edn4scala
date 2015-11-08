package net.cabworks.edn4scala

import java.io.StringWriter
import clojure.lang._
import scala.collection.JavaConverters._

/**
 * Created by cab on 09/10/2015.
 */
object EdnParser {
  type Expr = Any

  sealed trait EdnExpr {
    def value : Expr
    def mdata : Map[Expr,Expr]
    override def equals(other : Any) = other match {
      case that : EdnExpr => this.value == that.value
      case _ => false
    }
  }



  //EdnNumber,EdnString,EdnBoolean,EdnMap, etc... could be derived from this to add meta data

  //Let's just implement the missing Keyword and Symbol types for now.
  case class EdnKeyword(ns: String,name : String,meta: Map[Expr,Expr]) extends EdnExpr {
    def mdata = meta
    override def value = toString
    def getName = name

    override def toString = {
      if (ns == "" || ns == null) ":"+name
      else ":"+ns +"/"+name
    }

    def this(ns : String, name : String) = this(ns, name, Map())
    def this(name : String) = this("", name, Map())
  }

  object EdnKeyword {
    def apply(ns : String, name : String) = new EdnKeyword(ns, name)
    def apply(name : String) = new EdnKeyword(name)
  }

  case class EdnSymbol(ns: String,name : String,meta: Map[Expr,Expr]) extends EdnExpr {
    def mdata = meta
    override def value = toString
    override
    def toString = {
      if (ns == "" || ns == null) name
      else ns +"/"+name
    }
    def this(name: String) = this("", name, Map())
    def this(ns : String, name : String) = this(ns, name, Map())
  }

  trait EdnSeq extends EdnExpr {
    def head: Expr
    def tail: EdnSeq
  }

  case class EdnList(intern: List[Expr],meta: Map[Expr,Expr]) extends EdnSeq {
    def mdata = meta
    override def value = toString
    override def toString = "Edn_" + intern

    override def head: Expr = intern.head

    override def tail: EdnSeq = EdnList(intern.tail)
  }

  case class EdnVector(intern: Vector[Expr],meta: Map[Expr,Expr]) extends EdnSeq {
    def mdata = meta
    override def value = toString
    override def toString = "Edn_" + intern

    override def head: Expr = intern.head
    override def tail: EdnSeq = EdnVector(intern.tail)
  }

  case class EdnSet(intern: Set[Expr], meta: Map[Expr, Expr]) extends  EdnSeq {
    def mdata = meta
    override def value = toString
    override def toString = "Edn_" + intern

    override def head: Expr = intern.head
    override def tail: EdnSeq = EdnSet(intern.tail)
  }

  case class EdnMap(intern: Map[Expr,Expr], meta: Map[Expr,Expr]) extends EdnSeq {
    def mdata = meta
    override def value = toString
    override def toString = "Edn_" + intern

    override def head: Expr = intern.head
    override def tail: EdnSeq = EdnMap(intern.tail)
  }

  object EdnList {
    def apply() = new EdnList(List(), Map())
    def apply(intern : List[Expr]) = new EdnList(intern,Map())
  }
  object EdnVector {
    def apply() = new EdnVector(Vector(), Map())
    def apply(intern : Vector[Expr]) = new EdnVector(intern,Map())
  }
  object EdnSet {
    def apply() = new EdnSet(Set(), Map())
    def apply(intern: Set[Expr]) = new EdnSet(intern, Map())
  }

  object EdnSymbol {
    def apply(name : String) = new EdnSymbol(name)
    def apply(ns : String, name : String) = new EdnSymbol(ns, name)
  }

  object EdnMap {
    def apply() = new EdnMap(Map(), Map())
    def apply(intern: Map[Expr,Expr]) = new EdnMap(intern, Map())
  }

  // converts EDN string into a Scala data structure
  // returns Scala Map, Set, Vector, etc...
  def readEdnString(str: String): Expr = {
    val javaEdn = RT.readString(str)
    java2scalaRec(javaEdn)
  }

  // converts a Scala data structure into an EDN string
  def writeEdnString(expr: Expr):String = {
    val prStr : clojure.lang.IFn = clojure.java.api.Clojure.`var`("clojure.core","pr-str")
    prStr.invoke(scala2JavaRec(expr)).asInstanceOf[String]
  }


  //convert java Map,Set,Vector into Scala equivalent
  // convertion is deep/recursive.
  def java2scalaRec(expr: Expr): Any = {
    expr match {
      case _ : clojure.lang.IPersistentVector => EdnVector(expr.asInstanceOf[java.util.List[Expr]].asScala.toVector.map(subexp => java2scalaRec(subexp)))
      case _ : clojure.lang.IPersistentList => EdnList(expr.asInstanceOf[java.util.List[Expr]].asScala.toList.map(subexp => java2scalaRec(subexp)))
      case _ : clojure.lang.IPersistentMap => EdnMap(expr.asInstanceOf[java.util.Map[Expr,Expr]].asScala.toMap.map{case (k,v) => (java2scalaRec(k),java2scalaRec(v))})
      case _ : clojure.lang.IPersistentSet => EdnSet(expr.asInstanceOf[java.util.Set[Expr]].asScala.toSet.asInstanceOf[Set[Expr]].map(subexp => java2scalaRec(subexp)))
      case _ : java.util.List[Expr @unchecked] => EdnList(expr.asInstanceOf[java.util.List[Expr]].asScala.toList.map(subexp => java2scalaRec(subexp)))
      case n : clojure.lang.Ratio => n.numerator.doubleValue() / n.denominator.doubleValue()
      case u : java.util.UUID => u
      case n: Number => n
      case s: String => s
      case b: Boolean => b
      case d : java.util.Date => d
      case null => null

      case kw : clojure.lang.Keyword => EdnKeyword(kw.getNamespace,kw.getName(), Map[Expr, Expr]())
      case sym : clojure.lang.Symbol => EdnSymbol(sym.getNamespace,sym.getName(), Map[Expr, Expr]())
      case _ => throw new Exception("expr=" + expr + " (" + expr.getClass + ") is not Iterable")
    }
  }

  //same as previous but the otherway around
  def scala2JavaRec(expr: Expr): Any = {
    expr match {
      case EdnKeyword(ns,name,_) => clojure.lang.Keyword.intern(ns,name)
      case EdnSymbol(ns,name,_) => clojure.lang.Symbol.create(ns,name)

      case m : Map[Expr @unchecked,Expr @unchecked] => PersistentHashMap.create(m.map{case (k,v) => (scala2JavaRec(k),scala2JavaRec(v))}.asJava)
      case s : Set[Expr @unchecked] => PersistentHashSet.create(s.map(subexp => scala2JavaRec(subexp)).toList.asJava)
      case v : Vector[Expr @unchecked] => clojure.lang.PersistentVector.create(v.map(subexp => scala2JavaRec(subexp)).asJava)
      case l : List[Expr @unchecked] => clojure.lang.PersistentList.create(l.map(subexp => scala2JavaRec(subexp)).asJava)

      case _ => expr
    }
  }


  // this is the binding of the the "pprint" Clojure function in Scala
  val pprint = {
    import clojure.java.api.Clojure._
    val REQUIRE = `var`("clojure.core","require")
    REQUIRE.invoke(read("clojure.pprint"))
    `var`("clojure.pprint","pprint")
  }

  // Finally:
  // this is the example of how to call the Clojure pprint function in Scala
  def prettyPrintScalaDataStructureAsEdn(data: Any): String = {
    val writer = new StringWriter()
    pprint.invoke(scala2JavaRec(data),writer) // Calling Clojure function "pprint"
    writer.toString
  }
}






