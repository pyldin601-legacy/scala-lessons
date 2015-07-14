package org.gemini.scala.task

/**
 * Created by roman on 10.07.15
 */
object Trees extends App {

  /**
   * Returns number of possible combinations of trees after felling
   * with equal distances.
   * @param totalTrees Trees count before felling
   * @param neededNumberOfTrees Trees count after felling
   * @return Count of combinations
   */
  def countCombinationsOfTrees(totalTrees: Int, neededNumberOfTrees: Int) =
    if (neededNumberOfTrees == 1) totalTrees
    else (0 to totalTrees - neededNumberOfTrees).
      map(x => neededNumberOfTrees + (neededNumberOfTrees - 1) * x).
      filter(_ <= totalTrees).map(totalTrees - _ + 1).
      sum

  println(countCombinationsOfTrees(1000, 250))

}
