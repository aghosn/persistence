package scala.reflect.persistence.test

import org.scalatest.FunSuite
import scala.reflect.persistence._

class NodeTreeTest extends FunSuite {

  val root = ParseTestTree.parse("n ( n n ( n n ) )")

  test("FlattenBFSwithIdxs") {
    val bfsWithIdx = root.get.flattenBFSIdx
    assert(bfsWithIdx.head.bfsIdx == 4 && bfsWithIdx.head.parentBfsIdx == 2)
    assert(bfsWithIdx(1).bfsIdx == 3 && bfsWithIdx(1).parentBfsIdx == 2)
    assert(bfsWithIdx(2).bfsIdx == 2 && bfsWithIdx(2).parentBfsIdx == 0)
    assert(bfsWithIdx(3).bfsIdx == 1 && bfsWithIdx(3).parentBfsIdx == 0)
    assert(bfsWithIdx(4).bfsIdx == 0 && bfsWithIdx(4).parentBfsIdx == -1)
  }

  test("getSubtreeOfBFS") {
    val bfsWithIdx = root.get.getSubBFS(4)
    assert(bfsWithIdx.head.bfsIdx == 3 && bfsWithIdx.head.parentBfsIdx == 2)
    assert(bfsWithIdx(1).bfsIdx == 2 && bfsWithIdx(1).parentBfsIdx == 0)
  }
}