package upickle.implicits.namedTuples
import utest.*

object BasicTests extends TestSuite {
  val expectedOutput = """[
    {"x":23,"y":7.5,"z":500000000000},
    {"name":"Alice","isHuman":true,"isAlien":false},
    {"arr":[1,2,3],"optionalAny":null,"optionalInt":42}
  ]""".replaceAll("\\s+", "")

  val tests = Tests {

    test("example") {
      import upickle.implicits.namedTuples.default.given

      val namedTuple = (foo = Seq(1, 2, 3), bar = "hello", qux = Some(42))
      val json = """{"foo":[1,2,3],"bar":"hello","qux":42}"""

      assert(upickle.default.write(namedTuple) == json)

      val deserialized =
        upickle.default.read[(foo: Seq[Int], bar: String, qux:Option[Int])](json)

      assert(deserialized == namedTuple)
    }

    test("default cake") {
      test("serialization with primitives and option") {
        import upickle.implicits.namedTuples.default.given
        val json = upickle.default.write(
          (
            (x = 23, y = 7.5, z = 500_000_000_000L),
            (name = "Alice", isHuman = true, isAlien = false),
            (arr = (1, 2, 3), optionalAny = None, optionalInt = Some(42))
          )
        ) // named tuple write
        assert(json == expectedOutput)
      }

      test("deserialization with primitives, seq and option") {
        import upickle.implicits.namedTuples.default.given
        val result = upickle.default.read[
          (
              (x: Int, y: Double, z: Long),
              (name: String, isHuman: Boolean, isAlien: Boolean),
              (
                  arr: Seq[Int],
                  optionalAny: Option[Int],
                  optionalInt: Option[Int]
              )
          )
        ](expectedOutput)

        assert(
          result == (
            (x = 23, y = 7.5, z = 500_000_000_000L),
            (name = "Alice", isHuman = true, isAlien = false),
            (arr = Seq(1, 2, 3), optionalAny = None, optionalInt = Some(42))
          )
        )
      }

      test("readwriter with primitives, seq and option") {
        import upickle.implicits.namedTuples.default.given
        type Schema = (
            (x: Int, y: Double, z: Long),
            (name: String, isHuman: Boolean, isAlien: Boolean),
            (arr: Seq[Int], optionalAny: Option[Int], optionalInt: Option[Int])
        )
        val rw = summon[upickle.default.ReadWriter[Schema]]

        val data: Schema = (
          (x = 23, y = 7.5, z = 500_000_000_000L),
          (name = "Alice", isHuman = true, isAlien = false),
          (arr = Seq(1, 2, 3), optionalAny = None, optionalInt = Some(42))
        )

        val json = upickle.default.write[Schema](data)(using rw)
        assert(json == expectedOutput)
        val result = upickle.default.read[Schema](json)(using rw)
        assert(result == data)
      }

      test("unhappy path reading when expected keys are missing") {
        import upickle.implicits.namedTuples.default.given
        val json = """{"bar": 23}"""
        val err = intercept[upickle.core.AbortException] {
          upickle.default.read[(foo: Boolean)](json, trace = false)
        }
        assert(err.getMessage.contains("missing keys in dictionary: foo"))
      }

      test("unhappy path reading when json is not an object") {
        import upickle.implicits.namedTuples.default.given
        val json = """[]"""
        val err = intercept[upickle.core.AbortException] {
          upickle.default.read[(foo: Boolean)](json, trace = false)
        }
        assert(err.getMessage.contains("expected dictionary got sequence"))
      }
    }

    test("legacy cake") {

      test("serialization with primitives and option [legacy]") {
        import upickle.implicits.namedTuples.legacy.given
        val json = upickle.legacy.write(
          (
            (x = 23, y = 7.5, z = 500_000_000_000L),
            (name = "Alice", isHuman = true, isAlien = false),
            (arr = (1, 2, 3), optionalAny = None, optionalInt = Some(42))
          )
        )
        assert(json == expectedOutput)
      }

      test("deserialization with primitives, seq and option [legacy]") {
        import upickle.implicits.namedTuples.legacy.given
        val result = upickle.legacy.read[
          (
              (x: Int, y: Double, z: Long),
              (name: String, isHuman: Boolean, isAlien: Boolean),
              (
                  arr: Seq[Int],
                  optionalAny: Option[Int],
                  optionalInt: Option[Int]
              )
          )
        ](expectedOutput)

        assert(
          result == (
            (x = 23, y = 7.5, z = 500_000_000_000L),
            (name = "Alice", isHuman = true, isAlien = false),
            (arr = Seq(1, 2, 3), optionalAny = None, optionalInt = Some(42))
          )
        )
      }

      test("readwriter with primitives, seq and option [legacy]") {
        import upickle.implicits.namedTuples.legacy.given
        type Schema = (
            (x: Int, y: Double, z: Long),
            (name: String, isHuman: Boolean, isAlien: Boolean),
            (arr: Seq[Int], optionalAny: Option[Int], optionalInt: Option[Int])
        )
        val rw = summon[upickle.legacy.ReadWriter[Schema]]

        val data: Schema = (
          (x = 23, y = 7.5, z = 500_000_000_000L),
          (name = "Alice", isHuman = true, isAlien = false),
          (arr = Seq(1, 2, 3), optionalAny = None, optionalInt = Some(42))
        )

        val json = upickle.legacy.write[Schema](data)(using rw)
        assert(json == expectedOutput)
        val result = upickle.legacy.read[Schema](json)(using rw)
        assert(result == data)
      }
    }

  }
}
