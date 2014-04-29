package scala.reflect.persistence

import scala.annotation.tailrec
import scala.reflect.internal.util.Position

object NodeTag extends Enumeration {
  val PackageDef, ClassDef, ModuleDef, ValDef, DefDef, TypeDef, LabelDef, Import, Template, Block, CaseDef, Alternative, Star, Bind, UnApply, ArrayValue, Function, Assign, AssignOrNamedArg, If, Match, Return, Try, Throw, New, Typed, TypeApply, Apply, ApplyDynamic, This, Select, Ident, ReferenceToBoxed, Literal, Annotated, SingletonTypeTree, SelectFromTypeTree, CompoundTypeTree, AppliedTypeTree, TypeBoundsTree, ExistentialTypeTree, TypeTree, Super, EmptyTree, Separator = Value

  def getIndex(s: NodeTag.Value): Int = {
    NodeTag.values.toList.sortBy(_.toString).indexOf(s)
  }

  def getVal(i: Int): NodeTag.Value = {
    (NodeTag.values.toList.sortBy(_.toString).toList)(i)
  }
}

case class NodeBFS(node: Node, bfsIdx: Int, parentBfsIdx: Int) {
  def addChild(nd: Node) = this.copy(node = this.node.copy(children = nd :: this.node.children))
  def equalsStructure(that: NodeBFS): Boolean = (this.node.tpe == that.node.tpe && this.bfsIdx == that.bfsIdx && this.parentBfsIdx == that.parentBfsIdx)
}

case class Node(tpe: NodeTag.Value, children: List[Node]) {
  import Enrichments._

  val childrenBFS: RevList[Node] = this.flattenBFS
  val childrenBFSIdx: RevList[NodeBFS] = this.flattenBFSIdx
  
  def addChildren(nd: List[Node]) = this.copy(children = nd ++ this.children)
  def getSubBFS(i: Int) = childrenBFSIdx.takeRight(i)
  def cleanCopy: Node = this.copy(children = Nil)
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
  
  /* TODO: perhaps dict can be a val passed to loop */
  /* Compute a dictionary for the compression algorithm, based on LZW and the tree in BFS order */
  def computeFreqs = {
    var dict: NodeDict = Map(this.getSubBFS(1) -> 0)
    @tailrec def loop(que: List[Node]): Unit = que match {
      case Nil => /* Nothing more to parse */
      case nd :: nds =>
        val bfs = nd.childrenBFSIdx
        val candidates = dict.keys.filter(cand => cand.matchBFS(bfs))
        val max = candidates./:(List[NodeBFS]())((x, y) => if (x.size > y.size) x else y)
        max.size match {
          case 0 => /* nothing found, we add the new node to dict and all its children to the queue */
            dict += (bfs.takeSubtree(1) -> 1)
            loop(nds ++ nd.children)
          case sz if sz == bfs.size => /* perfect subtree matching */
            dict += (max -> (dict(max) + 1)) /* update the frequency of max in dict */
            loop(nds)
          case sz => /* the match does not cover all the subtree */
            val nwst = bfs.takeSubtree(sz + 1)
            val nwnd = nwst.head.copy(bfsIdx = 0, parentBfsIdx = -1)
            dict += (nwst -> 1) /* add max + one more node */
            dict.keys.find(k => k.size == 1 && k.head.equalsStructure(nwnd)) match{
              case None => dict += ((nwnd :: Nil) -> 1) /* Add the node to dict */
              case Some(ent) => dict+= (ent -> (dict(ent) + 1))
            } 
            candidates foreach (cdt => dict += (cdt -> (dict(cdt) + 1))) /* update counter for all prefix trees */
            loop(nds ++ nwst.subRoots.map(_._1)) /* add children of bfs.take(max.size + 1) to que */
        }
    }
    loop(this :: Nil)
    dict
  }

}

/* Companion object */
object Node {
  def apply(tpe: NodeTag.Value) = new Node(tpe, Nil)
  def empty = new Node(NodeTag.EmptyTree, Nil)
  def separator = new Node(NodeTag.Separator, Nil)
}

