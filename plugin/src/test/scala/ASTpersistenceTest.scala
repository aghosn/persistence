package scala.reflect.persistence.test

import org.scalatest.FunSuite
import scala.reflect.persistence._
import java.io.File
import java.io.DataOutputStream
import java.io.DataInputStream
import java.io.FileOutputStream
import java.io.FileInputStream

class ASTpersistenceTest extends FunSuite {
  var count = 0
  def compWriteReadDecomp(treeStr: String) {
    val tree = ParseTestTree.parse(treeStr).get

    count += 1
    val file = new File("WriteTest"+count)
    val compressor = new AstCompressor(new DataOutputStream(new FileOutputStream(file)))
    val decompressor = new AstDecompressor(new DataInputStream(new FileInputStream(file)))

    compressor(tree)
    val recupTree = decompressor()

    assert(tree == recupTree)
  } 

  test("First Tree"){
    val treeStr = "c (m (v v (c (m v v) c (m v v))) m(v v (c c)) c (m (v v (c (m v v) c (m v v))) m(v v (c c)) ))"
    compWriteReadDecomp(treeStr)

  }

  test("Second Tree"){
   val treeStr = "c (m (v v (c (m v v) c (m (v v)))) m(v v (c c)))"
   compWriteReadDecomp(treeStr)

  }

  test("Third Tree"){
    val treeStr = "c (n (m m v) m ( v v v v v ) m ( v v ) m (c v))"
    compWriteReadDecomp(treeStr)

  }

  test("Fourth Tree"){
    val treeStr = "n ( n ( n n ) n )"
    compWriteReadDecomp(treeStr)

  }

  test("Fifth Tree"){
   val treeStr = "m (c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) c (v v) )"
   compWriteReadDecomp(treeStr)
  }

  test("Sixth Tree"){
    val treeStr ="p (c (v v v v m (v v m (v v v m (v n) ) v m (c c (v v m) ) ) t (v v) c (v c) ) m m) "
    compWriteReadDecomp(treeStr)
  }

  test("Seventh Tree"){
    val treeStr = "c (m (v v (c (m v v) c (m v v))) m(v v (c c)) c (m (v v (c (m v v) c (m v v))) m(v v (c c)) ))"
    compWriteReadDecomp(treeStr)
  }
}
