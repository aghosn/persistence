package scala.reflect.persistence.test

import org.scalatest.FunSuite
import scala.reflect.persistence._
import scala.reflect.persistence.Enrichments._
import java.io.DataOutputStream
import java.io.FileOutputStream
import java.io.File

class AstCompressorTest extends FunSuite {

  /* Tests concerning computeFreqs */

  val compressor = new AstCompressor(null)

  test("parseTreeTest1") {
    val tree = ParseTestTree.parse("n ( n ( n n ) n )").get
    val exploitableDict = tree.computeFreqs.testingDict
    assert(exploitableDict.size == 2)
    assert(exploitableDict.head._2 == 5)
    assert(exploitableDict.tail.head._2 == 1)
  }

  test("parseTreeTest2") {
    val tree = ParseTestTree.parse("c (n (m m v) m ( v v v v v ) m ( v v ) m (c v))").get
    val exploitableDict = tree.computeFreqs.testingDict
    assert(exploitableDict.size == 8)
  }

  test("parseTreeTest3") {
    val tree = ParseTestTree.parse("c (v v c (v v) c(v v) c(v v))").get
    val exploitableDict = tree.computeFreqs.testingDict
    assert(exploitableDict.size == 4)
    val entry = MetaEntry(NodeTag.ValDef, 0, -1)
    assert(exploitableDict.contains(List(entry)))
    assert(exploitableDict(List(entry)) == 3)
    assert(exploitableDict.keys.exists(_.size == 3))
    assert(exploitableDict(exploitableDict.keys.find(_.size == 3).get) == 3)
  }

  test("DoublePattern") {
    val tree = ParseTestTree.parse("c (m (v v (c (m v v) c (m (v v)))) m(v v (c c)))").get
    val dict = tree.computeFreqs.testingDict
    println(dict)
    assert(dict.values.toList.count(_ == 2) == 1)
    assert(dict.values.toList.count(_ == 1) == 4)
  }

  /* Tests concerning the dictionary actually used for compression */

  /* Visual test */
  test("DictTreeTest1") {
    val tree = ParseTestTree.parse("c (m (v v (c (m v v) c (m v v))) m(v v (c c)) c (m (v v (c (m v v) c (m v v))) m(v v (c c)) ))").get
    val splitTree = compressor.splitTree(tree)
    println("Used Dictionary:")
    splitTree._1.testingDict foreach (println(_))
    println("Edges:")
    println(splitTree._3)
    assert(splitTree._3.head._1 == -1, "The edge reference of the first encoded subtree should always be -1.")
    val nbOcc = splitTree._1.values.sum
    assert(splitTree._2.size == nbOcc, "the number of occurences should be the same as the sum of the frequencies of the dicitonary.")
  }

  /* Visual test */
  test("HuffmanGenerationTest1") {
    val tree = ParseTestTree.parse("c (m (v v (c (m v v) c (m v v))) m(v v (c c)) c (m (v v (c (m v v) c (m v v))) m(v v (c c)) ))").get
    val splitTree = compressor.splitTree(tree)
    val hufCodes = compressor.genHuffman(splitTree._1)
    println("Huffman codes used for compression:")
    hufCodes.values foreach (c => println(c.map(v => v.toInt)))
    println("Bit string for the compressed tree:")
    compressor.encodeOccs(splitTree._2, hufCodes) foreach (print(_))
    println()
    assert(splitTree._1.size == hufCodes.size, "Wrong size of Huffman codes !")

  }

  /* Visual test */
  test("HuffmanGenerationTest2") {
    val tree = ParseTestTree.parse("n ( n ( n n ) n )").get
    val splitTree = compressor.splitTree(tree)
    println("Original dict:")
    println(splitTree._1)
    val hufCodes = compressor.genHuffman(splitTree._1)
    println("Huffman codes used for compression:")
    hufCodes.values foreach (c => println(c.map(v => v.toInt)))
    println("Bit string for the compressed tree:")
    compressor.encodeOccs(splitTree._2, hufCodes) foreach (print(_))
    println()
    assert(splitTree._1.size == hufCodes.size, "Wrong size of Huffman codes !")
  }
}
