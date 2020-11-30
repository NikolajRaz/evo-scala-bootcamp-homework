package akka

import akka.actor.{Actor, ActorRef, Props}

object BinaryTreeNode {
  private sealed trait Position

  private case object Left extends Position
  private case object Right extends Position

  def props(elem: Int, initiallyRemoved: Boolean): Props =
    Props(new BinaryTreeNode(elem, initiallyRemoved))
}

final class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean)
    extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet.Operation._
  import BinaryTreeSet.OperationReply._

  private var subtrees = Map[Position, ActorRef]()
  private var removed = initiallyRemoved

  override def receive: Receive = {
    case v: Insert   => doInsert(v)
    case v: Contains => doContains(v)
    case v: Remove   => doRemove(v)
  }

  private def doInsert(m: Insert): Unit = {
    if (m.elem == elem) {
      removed = false
      m.requester ! OperationFinished(m.id)
    } else {
      val pos = if (m.elem < elem) Left else Right
      subtrees.get(pos) match {
        case Some(value) => value ! m
        case None =>
          subtrees = subtrees.updated(
            pos,
            context.actorOf(props(m.elem, initiallyRemoved = false))
          )
          m.requester ! OperationFinished(m.id)
      }
    }
  }

  private def doContains(m: Contains): Unit = {
    if (m.elem == elem)
      m.requester ! ContainsResult(m.id, !removed)
    else {
      val pos = if (m.elem < elem) Left else Right
      subtrees.get(pos) match {
        case Some(value) => value ! m
        case None        => m.requester ! ContainsResult(m.id, result = false)
      }
    }
  }

  private def doRemove(m: Remove): Unit = {
    if (m.elem == elem) {
      removed = true
      m.requester ! OperationFinished(m.id)
    } else {
      val pos = if (m.elem < elem) Left else Right
      subtrees.get(pos) match {
        case Some(value) => value ! m
        case None        => m.requester ! OperationFinished(m.id)
      }
    }
  }
}
