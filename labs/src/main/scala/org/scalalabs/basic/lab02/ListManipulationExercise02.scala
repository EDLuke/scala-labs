package org.scalalabs.basic.lab02

import scala.collection.mutable.ListBuffer
import sys._

object ListManipulationExercise02 {

  /**
   * Find the maximum element in a list, e.g. maxElementInList(List(1,9,3,5)) == 9
   * As usual, various ways exist: pattern matching, folding, ...
   */
  def maxElementInList(l: List[Int]): Int = {
    l.foldLeft(Int.MinValue){
      (a, b) =>
        if (a < b)
          b
        else
          a
    }
  }

  /**
   * Calculate the sum of the equally position elements
   * of the two list
   */
  def sumOfTwo(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match{
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (xs, ys) => xs.head + ys.head :: sumOfTwo(xs.tail, ys.tail)
    }
  }

  /**
   *  For this exercise preferably make use of the sumOfTwo
   * method above
   */
  def sumOfMany(l: List[Int]*): List[Int] = {
    def sumOfManyHelper(l: List[List[Int]]) : List[Int] = {
      l match {
        case head :: tail => sumOfTwo(head, sumOfManyHelper(tail))
        case Nil => Nil
      }
    }
    sumOfManyHelper(l.toList)
  }

  case class Person(age: Int, firstName: String, lastName: String)

  /**
   * The following method is implemented in the most in-elegant way we could think of.
   * The idea is to re-write the method into more functional style. In the end, you
   * may be able to achieve the same functionality as implemented below
   * in a one-liner.
   */
  def separateTheMenFromTheBoys(persons: List[Person]): List[List[String]] = {
    val (boys, men) = persons.partition(_.age < 18)
    def sortByAgeGetFirstName(persons: List[Person]) : List[String] = persons.sortBy(_.age).map(_.firstName)
    List(sortByAgeGetFirstName(boys), sortByAgeGetFirstName(men))
  }

}