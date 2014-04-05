package scala.reflect.persistence

import scala.annotation.tailrec
import scala.reflect.internal.util.Position

/* Enum for the tree types */
object TreeTpe extends Enumeration {
  val PackageDef, ClassDef, ModuleDef, ValDef, DefDef, TypeDef, LabelDef, Import, Template, Block, CaseDef, Alternative, Star, Bind, UnApply, ArrayValue, Function, Assign, AssignOrNamedArg, If, Match, Return, Try, Throw, New, Typed, TypeApply, Apply, ApplyDynamic, This, Select, Ident, ReferenceToBoxed, Literal, Annotated, SingletonTypeTree, SelectFromTypeTree, CompoundTypeTree, AppliedTypeTree, TypeBoundsTree, ExistentialTypeTree, TypeTree, Super, EmptyTree, Separator = Value
}

/* Store an node with its bfs index and its parent bfs index */
case class NodeBFS(node: Node, bfsIdx: Int, parentBfsIdx: Int) {

  def :=:(other: NodeBFS): Boolean = (this.node :=: other.node && this.bfsIdx == other.bfsIdx && this.parentBfsIdx == other.parentBfsIdx)

  def addChild(nd: Node) = this.copy(node = this.node.copy(children = nd :: this.node.children))
}

/* Represents a node in the tree */
/* TODO: unfortunately, Constant is dependent of global. It's why it's an Any here. */
case class Node(tpe: TreeTpe.Value, mods: Option[ModifiersNode], sels: List[SelectorNode], children: List[Node], var pos: Option[Position] = None, value: Option[Any] = None) {
  import Implicits._

  val childrenBFS: RevList[Node] = this.flattenBFS
  val childrenBFSIdx: RevList[NodeBFS] = this.flattenBFSIdx
  
  def addChildren(nd: List[Node]) = this.copy(children = nd ++ this.children)

  /* Return a subtree of 'this' with only n children, as a list of NodeBFS */
  def getSubBFS(i: Int) = childrenBFSIdx.reverse.take(i).reverse

  /*TODO never called, might be useful later*/
  /* Return a subtree of 'this' with only the n children traversed in BFS order */
  def getSubtree(i: Int): Node = getSubBFS(i).toTree.get

  /* Return a version of the node without any child */
  def cleanCopy: Node = this.copy(children = Nil)

  /* TODO: figure out exactly what we need to compare */
  def :=:(n: Node): Boolean = (this.tpe == n.tpe && this.mods == n.mods)

  def flattenBFS = {
    @tailrec def loop(queue: List[Node], acc: RevList[Node]): RevList[Node] = queue match {
      case n :: ns => loop(ns ::: n.children, n.children.reverse ::: acc)
      case Nil => acc
    }
    loop(this :: Nil, this :: Nil)
  }

  /* Flatten the tree in BFS order, with nodes zipped with their indexes and their parent's indexes in BFS order */
  def flattenBFSIdx = {
    var count = 0
    def incr = { count += 1; count }
    @tailrec def loop(q: List[(Node, Int)], acc: RevList[NodeBFS]): RevList[NodeBFS] = q match {
      case Nil => acc
      case n :: ns =>
        val children = n._1.children map ((_, incr))
        loop(ns ::: children, children.map(x => NodeBFS(x._1, x._2, n._2)).reverse ::: acc)
    }
    loop((this, 0) :: Nil, NodeBFS(this, 0, -1) :: Nil)
  }

  def setPos(pos: Position) = this.pos = Some(pos)
}

/* Companion object */
object Node {
  def apply(tpe: TreeTpe.Value, mods: Option[ModifiersNode], children: List[Node]) = new Node(tpe, mods, Nil, children)
  def apply(tpe: TreeTpe.Value, children: List[Node]) = new Node(tpe, None, Nil, children)
  def apply(tpe: TreeTpe.Value, sels: List[SelectorNode], children: List[Node]) = new Node(tpe, None, sels, children)
  def apply(tpe: TreeTpe.Value, value: Any) = new Node(tpe, None, Nil, Nil, None, Some(value))
  def empty = new Node(TreeTpe.EmptyTree, None, Nil, Nil)
  def separator = new Node(TreeTpe.Separator, None, Nil, Nil)
}

case class ModifiersNode(flags: Long, annotations: List[Node])
case class SelectorNode(namePos: Int, renamePos: Int)
