package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  val persister = context.actorOf(persistenceProps)

  // We need to join to arbiter
  arbiter ! Join

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Insert(key: String, value: String, id: Long) =>
    {
      kv += (key -> value)

      replicators foreach (replicator => replicator ! Replicate(key,Some(value),id))

      sender ! OperationAck(id)
    }

    case Remove(key: String, id: Long) =>
    {
      kv -= (key)

      replicators foreach (replicator => replicator ! Replicate(key,None,id))

      sender ! OperationAck(id)
    }

    case Get(key: String, id: Long) =>
    {
      sender ! GetResult(key, kv.get(key), id)
    }

    case Replicas(replicas: Set[ActorRef]) =>
    {
      val diff = (replicas -- secondaries.keySet) - self
      diff foreach( replica => {
        val replicator = context.actorOf(Replicator.props(replica))
        secondaries += (replica -> replicator)
        replicators += replicator
        kv foreach { case (k,v) => replicator ! Replicate(k,Some(v),0) }
      } )
    }

    case Replicated(key: String, id: Long) =>
    {
      // TODO: what to do here?
      println(s"Replicated $key with id $id")
    }
  }

  /* TODO Behavior for the replica role. */
  var expectedSeq = 0L

  val replica: Receive = {
    case Get(key: String, id: Long) =>
    {
      sender ! GetResult(key, kv.get(key), id)
    }

    case Snapshot(key: String, valueOption: Option[String], seq: Long) =>
    {
      if (seq < expectedSeq) sender ! SnapshotAck(key, seq)
      if (seq == expectedSeq) {
        persister ! Persist(key, valueOption, seq)
        // TODO: guardar que se envió un llamado al persister, y esperar la respuesta. manejar el timeout

        if (valueOption.isEmpty) kv -= key else kv += (key -> valueOption.get)
        expectedSeq += 1
        sender ! SnapshotAck(key, seq)
      }
    }
  }

}

