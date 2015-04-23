package com.sparkydots.sort

import com.sparkydots.sort.mergers.MergeSorting
import com.sparkydots.sort.mergers.impl.{LinearMerger, OddEvenMerger, BitonicMerger}
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.util.Random

object MyBitonicSorter extends BitonicMerger with MergeSorting
object MyOddEvenSorter extends OddEvenMerger with MergeSorting
object MyLinearSorter extends LinearMerger with MergeSorting

class TestBitonic extends FlatSpec with Matchers {

  "Bitonic" should "work" in {

    val d = 7
    val N = math.pow(2, d).toInt


    (1 to 10).foreach { i =>

      val a = Seq.fill(N)(Random.nextInt(500))
      val expectedOrder = a.sorted

      MyBitonicSorter.sort(a) shouldBe expectedOrder
      MyLinearSorter.sort(a) shouldBe expectedOrder
      MyOddEvenSorter.sort(a) shouldBe expectedOrder

    }
  }

}



