package org.gemini.scala.fun

/**
 * Created by Roman on 25.06.2015.
 */


object DoneIsBetter extends App {

  object Done {
    def is(a: better.type): Done.type = { Done.this }
    def than(b: perfect.type): String = "You're right, dude! ;)"
  }

  object better
  object perfect

  println(Done is better than perfect)

}
