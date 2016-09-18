import scala.collection.mutable

/**
  * Created by kostya on 28/07/2016.
  */
object Probe extends App {

  def isShuffle(s1: String, s2: String, sh: String) = {

    val cache = mutable.Set[(String, String)]()

    def isSh(s1: String, s2: String, sh: String):Boolean = {
      if (cache.contains((s1,s2))) false
      else if (sh.length == 0) true
      else {
        if (s1.length > 0 && s1(0) == sh(0) && isSh(s1.substring(1), s2, sh.substring(1))) true
        else if (s2.length >0 && s2(0) == sh(0) && isSh(s1, s2.substring(1), sh.substring(1))) true
        else {
          cache.add((s1,s2))
          false
        }
      }
    }

    if (s1.length + s2.length != sh.length) false
    else isSh(s1, s2, sh)
  }

  println( isShuffle("ab", "ae", "aeab") )

  def zz: Unit = {
    val date = """(\d\d\d\d)-(\d\d)-(\d\d)""".r
    val embeddedDate = date.unanchored // to match anywhere inside the string
    val r = "2004-01-20" match {
        case date(year, _*) => s"$year was a good year for PLs."
      }
    println(r)
  }

  println( spl("we test   coders"))

  def spl(s: String ) = {
    s.split(' ').filter(_.nonEmpty).map( _.reverse  ).mkString(",")
  }


  def digits(N: Int) = {
    var f: BigInt = if (N > 0) 1 else 0
    for (i <- 2 to N) f = f * i
    var sum = 0
    val ten = BigInt(10)
    while (f > 0) {
      val (div, rem) = f /% ten
      sum = sum + rem.toInt
      f = div
    }
    sum
  }

  println( digits(0) )
  println( digits(1) )
  println( digits(2) )
  println( digits(14) )
  //println( digits(2000) )

}
