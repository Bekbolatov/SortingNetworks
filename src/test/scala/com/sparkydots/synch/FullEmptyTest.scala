package com.sparkydots.synch

import java.util.concurrent.{LinkedBlockingQueue, CountDownLatch}

import org.scalatest.{Matchers, FlatSpec}


class FullEmptyTest extends FlatSpec with Matchers {

  val free: Array[FE[Int]] = Array.fill(8)(new FE[Int])

  val x = new FE[Int]

  "FE" should "work" in {

    (0 until 8).foreach { i =>
      free(i) := i
    }

    (0 until 8).foreach { i =>
      new Thread(new Counter(free, i, 8)).start()
    }

  }



  it should "write" in {

    val L = new CountDownLatch(6)

    val t = new Thread(new FR(x, L))
      t.start()


    new Thread(new FW(x, 6, L)).start()

    new Thread(new FW(x, 7, L)).start()
    new Thread(new FW(x, 8, L)).start()

    new Thread(new FR(x, L)).start()
    new Thread(new FR(x, L)).start()

    new Thread(new FR(x, L)).start()
    new Thread(new FR(x, L)).start()
    new Thread(new FR(x, L)).start()

    new Thread(new FW(x, 9, L)).start()
    new Thread(new FW(x, 10, L)).start()
    new Thread(new FW(x, 11, L)).start()

    println("waiting")
    //L.await()
    println("hello")


    t.join()

  }


  it should "wait" in {
    val queue = new LinkedBlockingQueue[Int]()
    //queue.take()

  }

}


class Counter[T](buf: Array[FE[Int]], myId: Int, total: Int) extends Runnable {
  def run(): Unit = {
    var n = myId
    var d = 1

    if (myId == 2)
      Thread.sleep(2000)

    while(n % 2 == 0 && d < total) {
      val a = buf(myId).read
      val b = buf(myId + d).read
      buf(myId) := a + b
      println(s" added $myId and ${myId + d}")
      n /= 2
      d *= 2
    }
    if (myId == 0) {
      val result = buf(0).read()
      println(s"Result = $result")
    }
  }
}


class FW[T](a: FE[T], x: T, L: CountDownLatch) extends Runnable {
  def run(): Unit = {
    Thread.sleep(2000)
    a := x
  }
}


class FR[T](a: FE[T], L: CountDownLatch) extends Runnable {
  def run(): Unit = {
    println("R:")
    val y = a.read()
    println("S:" + y)
    L.countDown()
  }
}

