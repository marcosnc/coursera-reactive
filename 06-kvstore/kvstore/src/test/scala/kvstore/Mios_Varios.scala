package kvstore

import akka.actor.{Props, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalactic.ConversionCheckedTripleEquals
import org.scalatest.{BeforeAndAfterAll, FunSuiteLike, Matchers}

class Mios_Varios extends TestKit(ActorSystem("Step1PrimarySpec"))
    with FunSuiteLike
        with BeforeAndAfterAll
    with Matchers
    with ConversionCheckedTripleEquals
    with ImplicitSender
    with Tools {

  override def afterAll(): Unit = {
    system.shutdown()
  }

  test("case1: Primary must start replication to new replicas")
  {
    val arbiter = system.actorOf(Props(classOf[Arbiter]), "case1-arbiter")

    val primary = system.actorOf(Replica.props(arbiter, Persistence.props(flaky = false)), "case1-primary")

    val client = session(primary)
    client.waitAck(client.set("k1", "v1"))

    val secondary = system.actorOf(Replica.props(arbiter, Persistence.props(flaky = false)), "case1-secondary")

    Thread.sleep(1000)

  }


}
