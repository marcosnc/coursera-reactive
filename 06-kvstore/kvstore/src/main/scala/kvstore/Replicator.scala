package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.duration._

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher
  import scala.language.postfixOps
  
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  val tick = context.system.scheduler.schedule(100 millis, 100 millis, self, "tick")

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]
  
  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  
  /* TODO Behavior for the Replicator. */
  def receive: Receive = {
    case replicate: Replicate =>
    {
      val seq = nextSeq
      acks += (seq -> (sender, replicate))
      self ! "tick"
    }

    case SnapshotAck(key: String, seq: Long) =>
    {
      acks.get(seq) match {
        case None => println(s"Invalid SnapshotAck found ($key,$seq)")
        case Some((originalSender: ActorRef, Replicate(key: String, _, id: Long))) =>
        {
          acks -= seq
          originalSender ! Replicated(key, id)
        }
      }
    }

    case "tick" =>
    {
      acks foreach{ case (seq, (_, Replicate(key, valueOption, id))) => {
        replica ! Snapshot(key, valueOption, seq)
      }}
    }
  }

}