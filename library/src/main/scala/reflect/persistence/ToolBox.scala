package scala.reflect.persistence

import java.io._
import scala.language.existentials

/* Fetch trees and subtrees from the decompressed AST. */
/* For now : return the entire AST, take a universe as argument */
class ToolBox(val u: scala.reflect.api.Universe) {
  import u._
  import Enrichments._
  
  /* General function returning the whole tree */
  def getTst(file: String): Tree = {
    
    val src: java.io.DataInputStream = new DataInputStream(this.getClass().getResourceAsStream(file))
    val bytes: List[Byte] = new XZReader(src)()
    src.close()
    val hasNames: Boolean = (bytes.head == 1.toByte)
    val decompressor: AstDecompressor = new AstDecompressor()
    val nodeTree = decompressor(bytes.tail)
    val toRead: List[Byte] = decompressor.getToRead 
    var names: Map[String, List[Int]] = Map()
    if(hasNames)
      names = (new NameDecompressor()(toRead))._1
    
    /* TODO: rebuilt the tree from each part, ASTs, Symbols, etc. */
    new TreeRecomposer[u.type](u)(DecTree(nodeTree, names))
  }

  def getMethodDef(file: String, name: String): Tree = {
   getElement(file, name, NodeTag.DefDef) 
  }

  def getValDef(file: String, name: String): Tree = {
    getElement(file, name, NodeTag.ValDef)
  }
  
  
  def getModuleDef(file: String, name: String): Tree = {
    getElement(file, name, NodeTag.ModuleDef)
  }

  def getClassDef(file: String, name: String): Tree = {
   getElement(file, name, NodeTag.ClassDef) 
  }
 
  def getTypeDef(file: String, name: String): Tree = {
   getElement(file, name, NodeTag.TypeDef) 
  }

  def getLabelDef(file: String, name: String): Tree = {
   getElement(file, name, NodeTag.LabelDef) 
  }

  def getElement(file: String, name: String, tpe: NodeTag.Value): Tree = {
    val (nodeTree, names) = nameBasedRead(file, name)
    val bfs: RevList[NodeBFS] = nodeTree.flattenBFSIdx
    val index: Int = findIndex(bfs, tpe, names(name))
    if(index == -1)
      throw new Exception(s"Error: ${name} is not defined here")
    val subtree: List[NodeBFS] = extractSubBFS(bfs.reverse.drop(index))
    new TreeRecomposer[u.type](u)(DecTree(subtree.toTree, names), index)
  }

  /*Find the bfs index of the element that corresponds to our search*/
  def findIndex(nodes: RevList[NodeBFS], tpe: NodeTag.Value, occs: List[Int]): Int = occs match{
    case o::os =>
      /*TODO check if not possible to get the element at index o instead*/
      val node: NodeBFS = nodes.find(_.bfsIdx == occs.head).get
      if(node.node.tpe == tpe)
        o
      else if (! NodeTag.isADefine(node.node.tpe))
        -1
      else 
        findIndex(nodes, tpe, os)
    case _ => 
      -1
  }
  /*Helper function that reads all the elements we need to reconstruct the tree*/
  def nameBasedRead(file: String, name: String): (Node, Map[String, List[Int]]) = {
    val src: java.io.DataInputStream = new DataInputStream(this.getClass().getResourceAsStream(file))
    val bytes: List[Byte] = new XZReader(src)()
    val hasNames: Boolean = (bytes.head == 1.toByte)
    if(!hasNames)
      throw new Exception("Error: names are not saved !")
    val decompressor: AstDecompressor = new AstDecompressor()
    val nodeTree: Node = decompressor(bytes.tail)
    val toRead: List[Byte] = decompressor.getToRead
    var names: Map[String, List[Int]] = (new NameDecompressor()(toRead))._1
    if(!names.contains(name))
      throw new Exception("Error: specified name doesn't exist")
    src.close()
    (nodeTree, names)
  }

  /*@warning must give the list of nodes in normal BFS and head is the node we need*/
  def extractSubBFS(nodes: List[NodeBFS]): RevList[NodeBFS] = {
    assert(!nodes.isEmpty)
    def loop(nds: List[NodeBFS], acc: RevList[NodeBFS]): RevList[NodeBFS] = nds match {
      case Nil => acc
      case n::ns if(!acc.exists(_.bfsIdx == n.parentBfsIdx)) => 
        acc
      case n::ns => 
        loop(ns, n::acc)
    }
    loop(nodes.tail, nodes.head::Nil)
  }
}
