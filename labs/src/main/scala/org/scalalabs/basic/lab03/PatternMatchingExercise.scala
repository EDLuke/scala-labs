package org.scalalabs.basic.lab03
import sys._
/**
 * This exercise introduces you to the powerful pattern matching features of Scala.
 *
 * Pattern matching can in its essence be compared to Java's 'switch' statement,
 * even though it provides many more possibilites. Whereas the Java switch statmenet
 * lets you 'match' primitive types up to int's, Scala's pattern matching goes much
 * further. Practically everything from all types of objects and Collections
 * can be matched, not forgetting xml and a special type of class called case classes.
 *
 * Pattern matching is also often used in combination with recursive algorithms.
 *
 * For this exercise exclusively use pattern matching constructs in order to make the
 * corresponding unit test work.
 *
 * Reference material to solve these exercises can be found here:
 * Pattern matching in general: http://programming-scala.labs.oreilly.com/ch03.html#PatternMatching
 * Pattern matching in combination with partial functions: http://programming-scala.labs.oreilly.com/ch08.html#PartialFunctions
 */

object PatternMatchingExercise {

  /**
   * ***********************************************************************
   *  pattern matching exercises
   * For expected solution see unittest @PatternMatchingExerciseTest
   * ***********************************************************************
   */

  def describeLanguage(s: String) = {
    s match {
      case "Java" | "Smalltalk" =>
        "OOP"
      case "Clojure" | "Haskell" =>
        "Functional"
      case "Scala" =>
        "Hybrid"
      case "C" =>
        "Procedural"
      case "Oz" =>
        "Unknown"
    }
  }

  def matchOnInputType(in: Any) = {
    in match {
      case s : String =>
        "A string with length " + s.length.toString
      case i : Int
        if i > 0 =>
          "A positive integer"
      case i : Int
        if i < 0 =>
          "A negative integer"
      case p : Person =>
        "A person with name: " + p.name
      case seq : Seq[Int]
        if seq.length > 10 =>
          "Seq with more than 10 elements"
      case seq : Seq[String] =>
        "first: " + seq(0) + ", second: " + seq(1) + ", rest: " + seq.slice(2, seq.length).toString()
      case opt : Option[Int] =>
        "A Scala Option subtype"
      case null =>
        "A null value"
      case _ =>
        "Some Scala class"
    }
  }

  def older(p: Person): Option[String] = {
    p match {
      case p : Person
        if p.age > 30 => Some(p.name)
      case _ => None
    }
  }

  /**
   * ***********************************************************************
   * Pattern matching with partial functions
   * For expected solution see @PatternMatchingExerciseTest
   * ***********************************************************************
   */

  val pf1: PartialFunction[String, String] = {
    case "scala-labs" => "something"
    case "stuff" => "something"
  }

  val pf2: PartialFunction[String, String] = {
    case "other stuff" => "something"
  }

  val pf3: PartialFunction[String, String] = {
    pf1 orElse pf2
  }

}

case class Person(name: String, age: Int)