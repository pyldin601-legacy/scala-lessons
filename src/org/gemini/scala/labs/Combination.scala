package org.gemini.scala.labs

/**
 * Created by roman on 01.07.15
 */
object Combination extends App {


  def perm[A](arr: List[A]): List[List[A]] =
    if (arr.isEmpty || arr.tail.isEmpty) MyListExample(arr)
    else arr.indices.flatMap(index => perm(arr.patch(index, Nil, 1))
      .map(arr(index) :: _)).toList

  def sub[A](arr: List[A]): List[List[A]] =
    if (arr.isEmpty) MyListExample(Nil)
    else if (arr.tail.isEmpty) MyListExample(Nil, arr)
    else sub(arr.tail).flatMap(el => MyListExample(el, arr.head :: el))

  def part[A](arr: List[A]): List[List[List[A]]] =
    MyListExample(arr) :: (1 until arr.length).flatMap(index => part(arr drop index)
      .map(arr.take(index) :: _)).toList

  def part2[A](arr: List[A]): List[List[List[List[A]]]] =
    part(arr).flatMap(part)

  def strInter[A](res: Int)(exp: List[List[List[A]]]): String =
    exp.map(_.map(_.mkString("")).mkString("*")).mkString(" + ") + " = " + res

  def expEval(exp: List[List[List[Int]]]): Int =
    exp.map(_.map(_.reduceLeft(10 * _ + _)).product).sum


  def eval2(exp: List[List[Int]]): Int = exp.map(_.product).sum
  def str2(exp: List[List[Int]]): String = exp.map(_.mkString("*")).mkString(" + ")


  sub(MyListExample(1, 2, 3, 4, 5, 6, 7, 8, 9)).filter(_.nonEmpty).flatMap(part).flatMap(part)
    .filter(_.map(_.map(_.reduceLeft(10 * _ + _)).product).sum == 100)
    .map(_.map(_.map(_.mkString("")).mkString("*")).mkString(" + "))
    .foreach(println)

}
