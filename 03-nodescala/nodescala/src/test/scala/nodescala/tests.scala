package nodescala

import rx.lang.scala.Observable

import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be completed") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be completed") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  test("Future.run should allow stopping the computation") {
    var aux = "not started"
    val working  = Future.run() { ct =>
      Future {
        while (ct.nonCancelled) {
          aux = "working"
        }
        aux = "done"
      }
    }
    Future.delay(3 seconds) onSuccess {
      case _ => working.unsubscribe()
    }
    Thread.sleep(1000)
    assert(aux === "working")
    Thread.sleep(4000)
    assert(aux === "done")
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }
  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

  test("delay should delay") {
    val delay = Future.delay(1 second)
    val t0 = System.currentTimeMillis()
    Await.result(delay, 10 second)
    val elapsed = System.currentTimeMillis() - t0
    assert(elapsed > 950)
    assert(elapsed < 5000)
  }

  test("continueWith should wait for the first future to complete") {
    val delay = Future.delay(1 second)
    val always = (f: Future[Unit]) => 42

    try {
      Await.result(delay.continueWith(always), 500 millis)
      assert(false)
    }
    catch {
      case t: TimeoutException => // ok
    }
  }

  test("Future.now should throw a NoSuchElementException when not completed") {
    val delay = Future.delay(1 second)

    try {
      delay.now
      assert(false)
    }
    catch {
      case t: NoSuchElementException => // ok
    }
  }

  test("My - always")
  {
    val myAlways = Future.always("Hola!")

    assert(Await.result(myAlways, 1 second) === "Hola!")
  }

  test("My - never")
  {
    val myNever = Future.never

    try
    {
      Await.result(myNever, 100 millis)
      fail()
    } catch {
      case e: TimeoutException => // ok
    }
  }

  test("My - all")
  {
    val myAll = Future.all(List(Future.always(1), Future.always(2)))

    assert(Await.result(myAll, 1 second) === List(1,2))
  }

  test("My - all 2")
  {
    val myDelay1 = Future.delay(1 second).continue(_ => "1 segundo")
    val myDelay2 = Future.delay(2 second).continue(_ => "2 segundos")
    val myAll = Future.all(List(myDelay1, myDelay2))

    val t0 = System.currentTimeMillis()
    assert(Await.result(myAll, 3 second) === List("1 segundo","2 segundos"))
    val elapsed = System.currentTimeMillis()-t0
    assert(elapsed >= 2000);
    assert(elapsed < 3000);
  }

  test("My - any")
  {
    val t0 = System.currentTimeMillis()
    val myDelay1 = Future.delay(1 second).continue(_ => "1 segundo")
    val myDelay2 = Future.delay(2 second).continue(_ => "2 segundos")
    val myAny = Future.any(List(myDelay1, myDelay2))

    assert(Await.result(myAny, 3 second) === "1 segundo")
    val elapsed = System.currentTimeMillis()-t0
    assert(elapsed >= 1000);
    assert(elapsed < 3000);
  }

  test("My - delay")
  {
    val myDelay = Future.delay(500 millis)
    val t0 = System.currentTimeMillis()
    Await.result(myDelay, 1 second)
    val elapsed = System.currentTimeMillis()-t0
    assert(elapsed >= 500);
    assert(elapsed < 1000);
  }

  test("My - userInput")
  {
    val myUserInput = Future.userInput("type something")
    val t0 = System.currentTimeMillis()
    try
    {
      Await.result(myUserInput, 1 second)
      fail()
    } catch {
      case e: TimeoutException => // ok
    }
    val elapsed = System.currentTimeMillis()-t0
    assert(elapsed >= 1000);
  }

  test("My - run") {
   var aux = "not started"
   assert(aux === "not started")
   val working  = Future.run() { ct =>
     Future {
       while (ct.nonCancelled) {
         aux = "working"
       }
       aux = "done"
     }
   }

   val myDelay = Future.delay(1 seconds)
   myDelay onSuccess {
     case _ => working.unsubscribe()
   }

   Thread.sleep(100)
   assert(aux === "working")

   Await.result(myDelay, 2 second)
   Thread.sleep(100)
   assert(aux === "done")
  }

  test("My - now")
  {
    val myDelay = Future.delay(1 second).continue(_ => "TEST")

    try
    {
      assert(myDelay.now === "TEST")
      fail()
    } catch {
      case e: NoSuchElementException => // ok
    }

    Await.result(myDelay, 2 second)
    assert(myDelay.now === "TEST")
  }

  test("My - continue")
  {
    val myDelayContinue = Future.delay(1 second).continue(_ => "TEST")

    assert(Await.result(myDelayContinue, 2 second) === "TEST")
  }

  test("My - continue 2")
  {
    val theException = new Exception("TEST")
    val myDelayContinue = Future.delay(1 second).continue(_ => throw theException)

    myDelayContinue onFailure {
      case theFailure => assert(theFailure.getMessage==="TEST")
    }
    myDelayContinue onSuccess {
      case value => {
        println(value)
        fail("The future should fail")
      }
    }
    assert(myDelayContinue.value===None)
    Thread.sleep(2000)
    assert(myDelayContinue.value.get === Failure(theException))
  }

  test("Observable") {
    val a: Observable[Int] = Observable.apply(1 to 10)


                     }

}




