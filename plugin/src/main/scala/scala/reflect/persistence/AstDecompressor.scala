package scala.reflect.persistence

import java.io.DataInputStream

class AstDecompressor(in: DataInputStream) {
  import Enrichments._

  /* TODO: should be either nested or private. Is public here for tests */
  def rebuiltTree(occs: List[List[NodeBFS]], edges: List[(Int, Int)]): Node = {
    def loop(revOccs: RevList[List[NodeBFS]], revEdges: RevList[(Int, Int)]): List[NodeBFS] = (revOccs, revEdges) match {
      case (x :: Nil, Nil) => x /* We have recomposed all the tree. NB: (-1, -1) for the root should not be there. */
      case (x :: xs, (idx, parentBFS) :: ys) =>
        val (bef :+ parent, aft) = xs.splitAt(xs.size - idx)
        loop(bef ++ (parent.append(x, parentBFS) :: aft), ys)
        
      case _ => sys.error("Mismatch bettwen the number of occurences and edges.")
    } 
    loop(occs.reverse, edges.reverse).toTree
  }
  
  /* TODO: should be either nested or private. Is public here for tests */
  /* Decode the occurrences from a list of bytes to a list of subtrees in BFS order */
  def decodeOccs(occs: List[Byte], revDict: RevHufDict): List[List[NodeBFS]] = {
    def loop(occs: List[Byte]): List[List[NodeBFS]] = occs match {
      case Nil => Nil
      case _ => 
        val (mtch, rest) = revDict.getMatch(occs)
        mtch :: loop(rest)
    }
    loop(occs)
  }
 
  //TODO check that it is better than while hasNext
  def inputOccs: List[Byte] = readBytes(in.readShort)

  def inputDict: RevHufDict = {
    var dict: RevHufDict = Map()
    while(in.available > 0){
      val sizeHuff: Int = in.readShort.toInt
      val huffcode = readBytes(sizeHuff)
      val nbNode: Int = in.readShort
      val ndBfs: List[NodeBFS] = 
        (for(i <- 1 to nbNode) 
          yield (NodeTag.getVal(in.readByte.toInt), in.readShort.toInt, in.readShort.toInt)).toList.map(i => NodeBFS(Node(i._1, Nil), i._2, i._3))
      dict += (huffcode -> ndBfs)
    }
    dict
  }

  def readBytes(size: Int): List[Byte] = {
    var bytes: List[Byte] = Nil
    for(i <- (0 until Math.ceil(size.toDouble / 8).toInt)){
      bytes  :+= in.readByte
    }
    bytes.map(decompressBytes(_)).reverse.flatten.drop((8 - (size % 8)) % 8)
  }

  def inputEdges: List[(Int, Int)] = {
    val size: Short = in.readShort
    (for(i <- 1 to size) yield (in.readShort.toInt, in.readShort.toInt)).toList
  }

  //Decompresses the byte into a list of 8 bytes
  def decompressBytes(byte: Byte): List[Byte] = {
    (0 to 7).map{ i => 
      if((byte & (1 << i)) != 0) 1.toByte
      else 0.toByte
    }.toList.reverse
  } 
  def apply(): Node = ???

}
