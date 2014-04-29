package scala.reflect.persistence.test

import org.scalatest.FunSuite
import scala.reflect.persistence._
import scala.reflect.persistence.Enrichments._

class AstDecompressorTest extends FunSuite {

  /* NB: assumes that the splitTree, genHuffman, encodeOccs are working, i.e. the compression itself. */

  val decompressor = new AstDecompressor(null)
  val compressor = new AstCompressor(null)

  def splitTreeForTest(treeStr: String) = {

    val tree = ParseTestTree.parse(treeStr).get
    val splitTree = compressor.splitTree(tree)
    val hufCodes = compressor.genHuffman(splitTree._1)
    val occs = compressor.encodeOccs(splitTree._2, hufCodes)
    (tree, occs, splitTree._2, hufCodes, splitTree._3)
  }

  test("Decoding occurrences using reversed HufDict") {
    val (tree, hufOccs, originOccs, dict, edges) = splitTreeForTest("c (m (v v (c (m v v) c (m v v))) m(v v (c c)) c (m (v v (c (m v v) c (m v v))) m(v v (c c)) ))")
    val originOccsRec = decompressor.decodeOccs(hufOccs, dict.map(_.swap))
    assert(originOccs == originOccsRec, "The decompressed subtrees should be equivalent to the original.")
  }

  test("Rebuilding a tree once compressed 1") {
    val (tree, hufOccs, originOccs, dict, edges) = splitTreeForTest("c (m (v v (c (m v v) c (m v v))) m(v v (c c)) c (m (v v (c (m v v) c (m v v))) m(v v (c c)) ))")
    val originOccsRec = decompressor.decodeOccs(hufOccs, dict.map(_.swap))
    val treeRec = decompressor.rebuiltTree(originOccsRec, edges.tail)
    assert(tree == treeRec, "The decompressed tree do not match the oroginal one.")
  }

  test("Rebuilding a tree once compressed 2") {
    val (tree, hufOccs, originOccs, dict, edges) = splitTreeForTest("p (c (v v v v m (v v m (v v v m (v n) ) v m (c c (v v m) ) ) t (v v) c (v c) ) m m) ")
    val originOccsRec = decompressor.decodeOccs(hufOccs, dict.map(_.swap))
    val treeRec = decompressor.rebuiltTree(originOccsRec, edges.tail)
    assert(tree == treeRec, "The decompressed tree do not match the oroginal one.")
  }
  
  test("Rebuilding a tree once compressed 3") {
    val (tree, hufOccs, originOccs, dict, edges) = splitTreeForTest("m (v v (m c c c c c c c c c c c) )")
    val originOccsRec = decompressor.decodeOccs(hufOccs, dict.map(_.swap))
    val treeRec = decompressor.rebuiltTree(originOccsRec, edges.tail)
    assert(tree == treeRec, "The decompressed tree do not match the oroginal one.")
  }
  
  test("Rebuilding a tree once compressed 4") {
    val (tree, hufOccs, originOccs, dict, edges) = splitTreeForTest("m (c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) )")
    val originOccsRec = decompressor.decodeOccs(hufOccs, dict.map(_.swap))
    val treeRec = decompressor.rebuiltTree(originOccsRec, edges.tail)
    assert(tree == treeRec, "The decompressed tree do not match the oroginal one.")
  }
}