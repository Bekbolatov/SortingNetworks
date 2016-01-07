package com.sparkydots.sort.network

import scala.annotation.tailrec

/**
 *
 * Calculation of partners in Batcher's Odd-Even merge network
 *
 * Renat Bekbolatov, Apr 21, 2015
 *
 */
object PartnerOddEven {

  /**
   *
   * Calculates partner in Batcher Odd-Even network.
   *
   * @param n node index: 0, 1, 2, 3, ... 2^d^-1
   * @param l merge stage: 1, 2, 3, ... d
   * @param p stage step: 1, 2, 3, ... l
   * @return Returns partner node, or self (n) if no partner for this step
   */
  def partner(n: Int, l: Int, p: Int): Int = {

    assert(p <= l, "p should be at most l")
    assert(p > 0, "p should be at least 1")
    assert(l > 0, "l should be at least 1")

    if (p == 1)
      n ^ (1 << (l - 1))
    else {
      val (scale, box) = (1 << (l - p), 1 << p)
      val sn = n / scale - (n / scale / box) * box

      if (sn == 0 || sn == box - 1) n
      else if (sn % 2 == 0) n - scale else n + scale
    }
  }

  def pairs(d: Int): Seq[Seq[Seq[(Int, Int)]]] = {
    assert(d > 0, "d must be at least 1")
    (1 to d).map { l =>
      (1 to l).map { p =>
        (0 until math.pow(2, d).toInt).map(n => {
          val np = partner(n, l, p)
          (math.min(n, np), math.max(n, np))
        }).toSet.toSeq.filterNot(p => p._1 == p._2).sortBy(_._1)
      }
    }
  }
}
