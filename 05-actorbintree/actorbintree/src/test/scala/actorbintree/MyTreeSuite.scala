/**
 * Copyright (C) 2009-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor.{ Props, ActorRef, ActorSystem }
import org.scalatest.{ BeforeAndAfterAll, FlatSpec }
import akka.testkit.{ TestProbe, ImplicitSender, TestKit }
import org.scalatest.Matchers
import scala.util.Random
import scala.concurrent.duration._
import org.scalatest.FunSuiteLike
import actorbintree.BinaryTreeSet._

class MyTreeSuite(_system: ActorSystem) extends TestKit(_system) with FunSuiteLike with Matchers with BeforeAndAfterAll with ImplicitSender
{

  def this() = this(ActorSystem("BinaryTreeSuite"))

  override def afterAll: Unit = system.shutdown()

  test("proper inserts and lookups")
  {
    val topNode = system.actorOf(Props[BinaryTreeSet])

    topNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, false))

    topNode ! Insert(testActor, id = 2, 1)
    topNode ! Contains(testActor, id = 3, 1)

    expectMsg(OperationFinished(2))
    expectMsg(ContainsResult(3, true))

    topNode ! Insert(testActor, id = 4, -1)
    topNode ! Contains(testActor, id = 5, -1)

    expectMsg(OperationFinished(4))
    expectMsg(ContainsResult(5, true))

    topNode ! Contains(testActor, id = 6, 0)
    expectMsg(ContainsResult(6, false))

    topNode ! Insert(testActor, id = 7, 0)
    topNode ! Contains(testActor, id = 8, 0)

    expectMsg(OperationFinished(7))
    expectMsg(ContainsResult(8, true))
  }

  test("insert, remove and lookups")
  {
    val topNode = system.actorOf(Props[BinaryTreeSet])

    topNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, false))

    topNode ! Insert(testActor, id = 2, 1)
    topNode ! Contains(testActor, id = 3, 1)

    expectMsg(OperationFinished(2))
    expectMsg(ContainsResult(3, true))

    topNode ! Remove(testActor, id = 4, 1)
    topNode ! Contains(testActor, id = 5, 1)

    expectMsg(OperationFinished(4))
    expectMsg(ContainsResult(5, false))
  }
}
