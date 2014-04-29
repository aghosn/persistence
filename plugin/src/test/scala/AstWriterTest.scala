package scala.reflect.persistence.test

import org.scalatest.FunSuite
import scala.reflect.persistence._
import scala.reflect.persistence.Enrichments._
import scala.reflect.persistence.Node._
import java.io.File
import java.io.DataOutputStream
import java.io.FileOutputStream
import java.io.DataInputStream
import java.io.FileInputStream

class AstWriterTest extends FunSuite {

  test("WritingTest") {
    val treeStr = "c (m (v v (c (m v v) c (m v v))) m(v v (c c)) c (m (v v (c (m v v) c (m v v))) m(v v (c c)) ))"
    val tree = ParseTestTree.parse(treeStr).get
    
    val file = new File("WritingTest.ast")
    val compressor = new AstCompressor(new DataOutputStream(new FileOutputStream(file)))
    val decompressor = new AstDecompressor(new DataInputStream(new FileInputStream(file)))
    
    val splitTree = compressor.splitTree(tree)
    val hufCodes = compressor.genHuffman(splitTree._1)
    val encOccs = compressor.encodeOccs(splitTree._2, hufCodes)
    
    compressor.outputOccs(encOccs)
    compressor.outputEdges(splitTree._3)
    compressor.outputDict(hufCodes)
    
    val dOccs = decompressor.inputOccs
    val dEdges = decompressor.inputEdges
    val dDict = decompressor.inputDict
    val transCodes = hufCodes.map{e => 
      (e._1.map(n => NodeBFS(Node(n.node.tpe, Nil), n.bfsIdx, n.parentBfsIdx)).toList, e._2)}.toMap
    assert(encOccs == dOccs, "Occurences do not match")
    assert(splitTree._3.tail == dEdges, "Edges do not match")
    assert(transCodes.map(_.swap) == dDict, "Dict do not match")
    
    file.delete()    
  }
}
