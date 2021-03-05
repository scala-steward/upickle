package ujson
import upickle.core.{ObjArrVisitor, Visitor}

import scala.annotation.{switch, tailrec}
import java.nio.ByteBuffer

import ujson.util.ByteBuilder

/**
 * Basic ByteBuffer parser.
 *
 * This assumes that the provided ByteBuffer is ready to be read. The
 * user is responsible for any necessary flipping/resetting of the
 * ByteBuffer before parsing.
 *
 * The parser makes absolute calls to the ByteBuffer, which will not
 * update its own mutable position fields.
 */
final class ByteBufferParser[J](src: ByteBuffer) extends ByteParser[J]{
  private[this] final val start = src.position()
  private[this] final val limit = src.limit() - start

  private[this] var lineState = 0
  protected[this] def line: Int = lineState

  protected[this] final def newline(i: Int) = { lineState += 1 }
  protected[this] final def column(i: Int) = i

  protected[this] final def close() = { src.position(src.limit) }
  protected[this] final def dropBufferUntil(i: Int): Unit = ()
  protected[this] final def elem(i: Int): Byte = src.get(i + start)

  protected[this] final def sliceString(i: Int, k: Int): CharSequence = {
    val len = k - i
    val arr = new Array[Byte](len)
    src.position(i + start)
    src.get(arr, 0, len)
    src.position(start)
    new String(arr, utf8)
  }

  protected[this] final def atEof(i: Int) = i >= limit
}

object ByteBufferParser extends Transformer[ByteBuffer]{
  def transform[T](j: ByteBuffer, f: Visitor[_, T]) = new ByteBufferParser(j).parse(f)
}
