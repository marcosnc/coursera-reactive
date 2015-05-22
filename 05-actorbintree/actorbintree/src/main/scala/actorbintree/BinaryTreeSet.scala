/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case op:Operation => root ! op
    case GC => {
      val newRoot = createRoot
      context.become(garbageCollecting(newRoot))
      root ! CopyTo(newRoot)
    }
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case CopyFinished => {
      // TODO: Improve this
      root = newRoot
      while(!pendingQueue.isEmpty) {
        val op = pendingQueue.head
        pendingQueue = pendingQueue.tail
        root ! op
      }
      context.become(normal)
    }
    case op:Operation => {
      pendingQueue = pendingQueue.enqueue(op)
    }
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Insert(requester, id, insertElem) =>
    {
      // TODO: Improve this
      if (insertElem == elem)
      {
        removed = false
        requester ! OperationFinished(id)
      }
      else
      {
        val position = if (insertElem < elem) Left else Right
        if (has(position))
          get(position) ! Insert(requester, id, insertElem)
        else
        {
          create(position, insertElem)
          requester ! OperationFinished(id)
        }
      }
    }

    case Contains(requester, id, insertElem) =>
    {
      // TODO: Improve?
      if (insertElem == elem)
        requester ! ContainsResult(id, !removed)
      else
      {
        val position = if (insertElem < elem) Left else Right
        if (has(position)) get(position) ! Contains(requester, id, insertElem)
        else requester ! ContainsResult(id, false)
      }
    }

    case Remove(requester, id, insertElem) =>
    {
      // TODO: Improve this
      if (insertElem == elem)
      {
        removed = true
        requester ! OperationFinished(id)
      }
      else
      {
        val position = if (insertElem < elem) Left else Right
        if (has(position))
          get(position) ! Remove(requester, id, insertElem)
        else
          requester ! OperationFinished(id)
      }
    }

    case CopyTo(treeNode: ActorRef) => {
      context.become(newState(subtrees.values.toSet, removed))
      if (!removed) treeNode ! Insert(self, 0, elem)
      if (has(Left)) get(Left) ! CopyTo(treeNode)
      if (has(Right)) get(Right) ! CopyTo(treeNode)
    }
  }

  def has(position:Position) = subtrees.contains(position)
  def get(position:Position) = subtrees(position)
  def create(position:Position, elem: Int) = subtrees += (position -> context.actorOf(props(elem, initiallyRemoved = false)))

  def newState(expected: Set[ActorRef], insertConfirmed: Boolean): Receive =
    if( expected.isEmpty && insertConfirmed ) {
      context.parent ! CopyFinished
      self ! PoisonPill
      normal
    } else {
      copying(expected, insertConfirmed)
    }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished(id) => context.become(newState(expected, true))
    case CopyFinished => context.become(newState(expected-sender(), insertConfirmed))
  }

}
