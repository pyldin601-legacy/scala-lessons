package org.gemini.scala.task

/**
 * Created by roman on 10.07.15
 */
object Trees extends App {

  def trees(total: Int, need: Int) =
    if (need == 1) total
    else (0 to total - need).map(x => need + ((need - 1) * x)).
      filter(_ <= total).map(total - _ + 1).sum

  println(trees(100, 20))

}
