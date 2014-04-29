package messy
import java.io.OutputStream
import java.io.InputStream
import java.io.FileOutputStream
import java.io.FileInputStream
import java.nio.ByteBuffer
import java.io.DataInputStream
import java.io.DataOutputStream
import scala.language.postfixOps

object Lzw {

  /* Dictionaries for compression and decrompression */

  val initialSize = 255 /* Initial size of the dictionary (ASCII), last one not included. */

  /* Creating the tables for the compression and decompression */
  var dictC = List range (0, initialSize) map (x => (List(x.toShort) -> x.toShort)) toMap
  var dictD = List range (0, initialSize) map (x => (x.toShort -> List(x.toShort))) toMap

  /**
   * @brief compress from in to out.
   */
  def compress(in: DataInputStream, out: DataOutputStream, buffer: List[Short] = Nil): Unit = if (in.available() > 0) in.readByte.toShort match {

    case x if dictC contains (buffer :+ x) => compress(in, out, buffer :+ x)

    case x =>
      dictC += ((buffer :+ x) -> (dictC.size + 1).toShort)
      out.write(shortToArray(dictC.get(buffer).get))
      compress(in, out, x :: Nil)
  }
  else if (!buffer.isEmpty) out.write(shortToArray(dictC.get(buffer).get)) /* Print the leftover */

  /**
   * @brief decompress from in to out.
   */
  def decompress(in: DataInputStream, out: DataOutputStream, buffer: List[Short] = Nil): Unit = if (in.available() > 0) {
    val dec = in.readShort match {

      case x if (dictD contains (x)) => dictD(x) /* The value is already in the dictionary */
      case x if x == dictD.size => (buffer :+ buffer(0)) /* The value is not in the dictionary : we add the first element of the buffer to the tail. */
    }

    dec foreach (out.write(_)) /* Print the decrypted values */
    dictD += ((dictD.size).toShort -> (buffer :+ dec(0))) /* update the dictionary */
    decompress(in, out, dec) /* recursion */
  }

  /**
   * @brief return an array containing the short (fixed size)
   */
  def shortToArray(x: Short) = {
    val buf = ByteBuffer.allocate(2)
    buf.putShort(x)
    buf.array()
  }

  def main(args: Array[String]) {
    
    if (args.length == 3) {
        

      val in = new DataInputStream(new FileInputStream(args(0)))
      val cmp = new DataOutputStream(new FileOutputStream(args(1)))

      compress(in, cmp)

      val enc = new DataInputStream(new FileInputStream(args(1)))
      val dec = new DataOutputStream(new FileOutputStream(args(2)))

      decompress(enc, dec)
    }
  }
}
