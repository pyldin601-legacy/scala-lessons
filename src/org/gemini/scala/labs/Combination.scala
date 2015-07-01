package org.gemini.scala.labs

/**
 * Created by roman on 01.07.15
 */
object Combination extends App {
  val elems: List[Int] = List(0, 1, 2)

  def perm[A](list: List[A]): List[List[A]] = {
    if (list.length == 0)
      List(list)
    else
      list.indices.flatMap(k => perm(list.patch(k, Nil, 1)).map(w => list(k) :: w)).toList
  }

  println(perm(elems))
}
