package org.gemini.scala.labs

/**
 * Created by Roman on 03.07.2015
 */
object Combo1 extends App {

  val list = List(1, 2, 3, 4, 5)

  def part[T](list: List[T]): List[List[List[T]]] = {
    List(list) :: (1 until list.length)
      .flatMap(i => part(list drop i).map(list.take(i) :: _)).toList
  }

  def eval(arg: List[List[Int]]): Int = arg.map(_.product).sum

  def display(arg: List[List[Int]]): String = arg.map(_.mkString("*")).mkString(" + ")

  def calc(arg: List[Int], value: Int): List[String] =
    part(list).filter(eval(_) == value).map(e => display(e) + " = " + value)

  calc(list, 15) foreach println

}