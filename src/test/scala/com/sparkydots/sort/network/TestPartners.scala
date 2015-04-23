package com.sparkydots.sort.network

import com.sparkydots.sort.network.PartnerOddEven._
import org.scalatest.{FlatSpec, Matchers}

class TestPartners extends FlatSpec with Matchers {

  def check(l: Int, p: Int) = (n: Int, m: Int) => partner(n, l, p) shouldBe m

  class Checker(l: Int, p: Int)(on: Int) {
    val c = check(l, p)
    def ~(to: Int) = c(on, to)
    def s = c(on, on)
  }

  "Partners" should "work 1, 1" in {

    (1 to 4).foreach{ d =>
      println(s" FOR d = $d")
      pairs(d).foreach { ls =>
        println()
        ls.foreach { ps =>
          println(ps)
        }
      }
      println()
    }

    (1 to 4).foreach{ d =>
      println(s" FOR d = $d")
      pairs(d).foreach { ls =>
        println()
        ls.foreach { ps =>
          println(ps)
        }
      }
      println()
    }


    implicit class C(on: Int) extends Checker(1, 1)(on)
    0 ~ 1
    1 ~ 0
    2 ~ 3
    9 ~ 8
    13 ~ 12
    14 ~ 15
  }

  it should "work 2, 1" in {
    implicit class C(on: Int) extends Checker(2, 1)(on)
    0 ~ 2
    1 ~ 3
    2 ~ 0
    4 ~ 6
    9 ~ 11
  }

  it should "work 2, 2" in {
    implicit class C(on: Int) extends Checker(2, 2)(on)
    0 ~ 0
    1 ~ 2
    2 ~ 1
    3 ~ 3
    5 ~ 6
    7 ~ 7
    8 ~ 8
    9 ~ 10
    10 ~ 9
    11 ~ 11
  }

  it should "work 3, 1" in {
    implicit class C(on: Int) extends Checker(3, 1)(on)
    2 ~ 6
  }

  it should "work 3, 2" in {
    implicit class C(on: Int) extends Checker(3, 2)(on)
    0 ~ 0
    1 ~ 1
    2 ~ 4
    3 ~ 5
    4 ~ 2
    5 ~ 3
    6 ~ 6
    7 ~ 7

    8 ~ 8
    9 ~ 9
    10 ~ 12
    11 ~ 13
    12 ~ 10
    13 ~ 11
    14 ~ 14
  }

  it should "work 3, 3" in {
    implicit class C(on: Int) extends Checker(3, 3)(on)
    0 ~ 0
    1 ~ 2
    2 ~ 1
    3 ~ 4
    4 ~ 3
    5 ~ 6
    6 ~ 5
    7 ~ 7
    8 ~ 8
    9 ~ 10
  }


}
