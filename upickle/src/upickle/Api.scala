package upickle



import jawn.{Facade, RawFContext, RawFacade}

import language.experimental.macros
import language.higherKinds
import scala.reflect.ClassTag
/**
 * An instance of the upickle API. There's a default instance at
 * `upickle.default`, but you can also implement it yourself to customize
 * its behavior. Override the `annotate` methods to control how a sealed
 * trait instance is tagged during reading and writing.
 */
trait Api extends Types with Implicits {
  def read[T: Reader](s: String) = {
    jawn.Parser.parseUnsafe(s)(implicitly[Reader[T]])
  }
  def write[T: Writer](t: T, indent: Int = -1) = {
    val out = new java.io.StringWriter()
    implicitly[Writer[T]].write(new Renderer(out, indent = indent), t)
    out.toString
  }
//  def annotate[V: ClassTag](rw: Reader[V], n: String): Reader[V]
//  def annotate[V: ClassTag](rw: Writer[V], n: String): Writer[V]
}

/**
 * The default way of accessing upickle
 */
object default extends AttributeTagged{

}
/**
 * An instance of the upickle API that follows the old serialization for
 * tagged instances of sealed traits.
 */
object legacy extends Api{
  def annotate[V: ClassTag](rw: Reader[V], n: String) = new TaggedReader[V] {
    def tags = Seq(n)

    def readers = Seq(rw)
  }

  def annotate[V: ClassTag](rw: Writer[V], n: String) = new TaggedWriter[V]{
    def tags = Seq(n)

    def write[R](out: Facade[R], v: V): R = {
      val ctx = out.arrayContext(-1).asInstanceOf[RawFContext[Any, R]]
      ctx.add(out.jstring(n, -1), -1)

      ctx.add(rw.write(out, v), -1)

      ctx.finish(-1)
    }
  }

  override def newTaggedReader[T](tags0: Seq[String], readers0: Seq[Reader[T]]) = new TaggedReaderImpl[T] {
    def tags = tags0
    def readers = readers0
  }
  type TaggedReader[T] = TaggedReaderImpl[T]
  trait TaggedReaderImpl[T] extends TaggedReader0[T]{
    def tags: Seq[String]
    def readers: Seq[Reader[T]]

    override def jstring(cs: CharSequence, index: Int) = cs.toString.asInstanceOf[T]
    override def arrayContext(index: Int) = new RawFContext[Any, T] {
      var typeName: String = null
      var delegate: Reader[_] = null
      var delegateCtx: RawFContext[_, T] = null

      var res: T = _
      def visitKey(s: CharSequence, index: Int): Unit = ???
      def facade =
        if (typeName == null) TaggedReaderImpl.this.asInstanceOf[RawFacade[Any]]
        else delegate.asInstanceOf[RawFacade[Any]]

      def add(v: Any, index: Int): Unit = {
        if (typeName == null){
          typeName = v.toString
          delegate = readers(tags.indexOf(typeName))
        }else{
          res = v.asInstanceOf[T]
        }
      }

      def finish(index: Int) = {
        res
      }

      def isObj = false
    }
  }
  def joinTagged[T: TaggedReader: TaggedWriter]: TaggedReadWriter[T] = new TaggedReaderImpl[T] with TaggedWriter[T] {
    def tags = implicitly[TaggedReader[T]].tags
    def readers = implicitly[TaggedReader[T]].readers
    override def jnull(index: Int) = implicitly[Reader[T]].jnull(index)
    override def jtrue(index: Int) = implicitly[Reader[T]].jtrue(index)
    override def jfalse(index: Int) = implicitly[Reader[T]].jfalse(index)

    override def jstring(s: CharSequence, index: Int) = implicitly[Reader[T]].jstring(s, index)
    override def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      implicitly[Reader[T]].jnum(s, decIndex, expIndex, index)
    }

    override def objectContext(index: Int) = implicitly[Reader[T]].objectContext(index)
    override def arrayContext(index: Int) = implicitly[Reader[T]].arrayContext(index)
    override def singleContext(index: Int) = implicitly[Reader[T]].singleContext(index)

    def write[R](out: Facade[R], v: T): R = {
      implicitly[Writer[T]].write(out, v)
    }
  }
}

/**
 * A `upickle.Api` that follows the default sealed-trait-instance-tagging
 * behavior of using an attribute, but allow you to control what the name
 * of the attribute is.
 */
trait AttributeTagged extends Api{
  def tagName = "$type"
  def annotate[V: ClassTag](rw: Reader[V], n: String) = new TaggedReader[V] {
    def tags = Seq(n)

    def readers = Seq(rw)
  }

  def annotate[V: ClassTag](rw: Writer[V], n: String) = new TaggedWriter[V]{
    def tags = Seq(n)

    def write[R](out: Facade[R], v: V): R = {
      val s = new java.io.StringWriter()
      val tree = rw.write(JsBuilder, v)
      val Js.Obj(kvs @ _*) = tree
      val tagged = Js.Obj((tagName -> Js.Str(n)) +: kvs: _*)
      JsVisitor.visit(tagged, out)
    }
  }

  override def newTaggedReader[T](tags0: Seq[String], readers0: Seq[Reader[T]]) = new TaggedReaderImpl[T] {
    def tags = tags0
    def readers = readers0
  }
  type TaggedReader[T] = TaggedReaderImpl[T]
  trait TaggedReaderImpl[T] extends TaggedReader0[T]{
    def tags: Seq[String]
    def readers: Seq[Reader[T]]

    override def jstring(cs: CharSequence, index: Int) = cs.toString.asInstanceOf[T]
    override def objectContext(index: Int) = Util.mapContext(JsObjR.objectContext(index)){ x =>
      val key = x.value.find(_._1 == tagName).get._2.str.toString
      val delegate = readers(tags.indexOf(key))

      val ctx = delegate.objectContext(-1)
      for((k, v) <- x.value if k != tagName){
        ctx.visitKey(k, -1)
        ctx.add(JsVisitor.visit(v, ctx.facade), -1)
      }
      ctx.finish(index)
    }
  }
  def joinTagged[T: TaggedReader: TaggedWriter]: TaggedReadWriter[T] = new TaggedReaderImpl[T] with TaggedWriter[T] {
    def tags = implicitly[TaggedReader[T]].tags
    def readers = implicitly[TaggedReader[T]].readers
    override def jnull(index: Int) = implicitly[Reader[T]].jnull(index)
    override def jtrue(index: Int) = implicitly[Reader[T]].jtrue(index)
    override def jfalse(index: Int) = implicitly[Reader[T]].jfalse(index)

    override def jstring(s: CharSequence, index: Int) = implicitly[Reader[T]].jstring(s, index)
    override def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      implicitly[Reader[T]].jnum(s, decIndex, expIndex, index)
    }

    override def objectContext(index: Int) = implicitly[Reader[T]].objectContext(index)
    override def arrayContext(index: Int) = implicitly[Reader[T]].arrayContext(index)
    override def singleContext(index: Int) = implicitly[Reader[T]].singleContext(index)

    def write[R](out: Facade[R], v: T): R = {
      implicitly[Writer[T]].write(out, v)
    }
  }
}

object json{
  val jsRW = upickle.default.macroRW0[Js.Value](implicitly, implicitly)
  def read(s: String) = jawn.Parser.parseUnsafe(s)(jsRW)
  def read(s: java.nio.ByteBuffer) = jawn.Parser.parseFromByteBuffer(s)(jsRW).get
  def read(s: java.io.File) = jawn.Parser.parseFromFile(s)(jsRW).get
  def write(t: Js.Value): String = {
    val out = new java.io.StringWriter()
    jsRW.write(new Renderer(out), t)
    out.toString
  }
}