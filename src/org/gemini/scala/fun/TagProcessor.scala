package org.gemini.scala.fun

/**
 * Created by Roman on 15.07.2015
 */
object TagProcessor extends App {

  val MASK_PREF = '{'
  val MASK_POST = '}'

  /**
   * This function is used for parsing text into HashMap
   * with keys extracted from pattern.
   *
   * @param pattern Text containing pattern
   * @param value Text that must be parsed
   * @param acc Accumulation map used in tail recursion
   * @return Map that contains parsed text into "key => value"
   */
  private def parseValueUsingPattern(pattern: String, value: String, acc: Map[String, String]): Map[String, String] = {
    if (pattern.isEmpty) acc
    else if (pattern.last == MASK_POST) {
      pattern.lastIndexOf(MASK_PREF) match {
        case -1 =>
          throw new RuntimeException("Unclosed pattern bracket")
        case i =>
          val key = pattern.substring(i + 1, pattern.length - 1).trim
          val supportRight = pattern.lastIndexOf(MASK_POST, i) match {
            case -1 => 0
            case e => e + 1
          }
          val support = pattern.substring(supportRight, i)
          if (support.isEmpty)
            acc + (key -> value)
          else
            value.lastIndexOf(support) match {
              case -1 => Map.empty
              case e => parseValueUsingPattern(pattern.substring(0, supportRight),
                value.substring(0, e),
                acc + (key -> value.substring(e + support.length)))
            }
      }
    } else if (pattern.last == value.last) {
      parseValueUsingPattern(pattern.init, value.init, acc)
    } else
      Map.empty
  }

  def mapStringByPattern(pattern: String, text: String): Map[String, String] = {
    if ((pattern zip pattern tail).exists(x => x._1 == '}' && x._2 == '{')) {
      throw new RuntimeException("Error in template: No support between '}' and '{'.")
    }
    parseValueUsingPattern(pattern, text, Map.empty)
  }

  println(mapStringByPattern(
    "/{artist}/{album}/{track_number}. {title}.{extension}",
    "/media/Music/Robert Miles/Some Album/01. Children.mp3"
  ))

}
