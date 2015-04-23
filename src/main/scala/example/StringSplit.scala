package example

import com.google.common.base.Splitter
import scala.collection.JavaConversions._


object StringSplit extends App {

  println("hello")

  val parts = Splitter.on(',')
    .trimResults()
    .omitEmptyStrings()
    .split("foo,bar,,   baz")

  parts.foreach { part => println(s"Part: $part")}

}