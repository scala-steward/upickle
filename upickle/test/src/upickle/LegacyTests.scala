package upickle
import utest._
import LegacyTestUtil.rw
import upickle.legacy.{ReadWriter => RW, Reader => R, Writer => W}
object LegacyTests extends TestSuite {

  val tests = Tests {
    'simpleAdt {
      implicit def ADT0rw: RW[ADTs.ADT0] = upickle.legacy.macroRW
      implicit def ADTarw: RW[ADTs.ADTa] = upickle.legacy.macroRW
      implicit def ADTbrw: RW[ADTs.ADTb] = upickle.legacy.macroRW
      implicit def ADTcrw: RW[ADTs.ADTc] = upickle.legacy.macroRW
      implicit def ADTdrw: RW[ADTs.ADTd] = upickle.legacy.macroRW
      implicit def ADTerw: RW[ADTs.ADTe] = upickle.legacy.macroRW
      implicit def ADTfrw: RW[ADTs.ADTf] = upickle.legacy.macroRW
      implicit def ADTzrw: RW[ADTs.ADTz] = upickle.legacy.macroRW

      * - rw(ADTs.ADT0(), """{}""")
      * - rw(ADTs.ADTa(1), """{"i":1}""")
      * - rw(ADTs.ADTb(1, "lol"), """{"i":1,"s":"lol"}""")

      * - rw(ADTs.ADTc(1, "lol", (1.1, 1.2)), """{"i":1,"s":"lol","t":[1.1,1.2]}""")
      * - rw(
        ADTs.ADTd(1, "lol", (1.1, 1.2), ADTs.ADTa(1)),
        """{"i":1,"s":"lol","t":[1.1,1.2],"a":{"i":1}}"""
      )

      * - rw(
        ADTs.ADTe(1, "lol", (1.1, 1.2), ADTs.ADTa(1), List(1.2, 2.1, 3.14)),
        """{"i":1,"s":"lol","t":[1.1,1.2],"a":{"i":1},"q":[1.2,2.1,3.14]}"""
      )

      * - rw(
        ADTs.ADTf(1, "lol", (1.1, 1.2), ADTs.ADTa(1), List(1.2, 2.1, 3.14), Some(None)),
        """{"i":1,"s":"lol","t":[1.1,1.2],"a":{"i":1},"q":[1.2,2.1,3.14],"o":[[]]}"""
      )
      val chunks = for (i <- 1 to 18) yield {
        val rhs = if (i % 2 == 1) "1" else "\"1\""
        val lhs = '"' + s"t$i" + '"'
        s"$lhs:$rhs"
      }

      val expected = s"""{${chunks.mkString(",")}}"""
      * - rw(
        ADTs.ADTz(1, "1", 1, "1", 1, "1", 1, "1", 1, "1", 1, "1", 1, "1", 1, "1", 1, "1"),
        expected
      )
    }

    'sealedHierarchy {
      // objects in sealed case class hierarchies should always read and write
      // the same way (with a tag) regardless of what their static type is when
      // written. This is feasible because sealed hierarchies can only have a
      // finite number of cases, so we can just check them all and decide which
      // class the instance belongs to.
      import Hierarchy._
      implicit def Brw: RW[B] = upickle.legacy.macroRW
      implicit def Crw: RW[C] = upickle.legacy.macroRW
      implicit def Arw: RW[A] = upickle.legacy.ReadWriter.merge(Crw, Brw)

      implicit def Zrw: RW[Z] = upickle.legacy.macroRW
      'shallow {
        * - rw(B(1), """["upickle.Hierarchy.B",{"i":1}]""")
        * - rw(C("a", "b"), """["upickle.Hierarchy.C",{"s1":"a","s2":"b"}]""")

        * - rw(AnZ: Z, """["upickle.Hierarchy.AnZ",{}]""")
        * - rw(AnZ, """["upickle.Hierarchy.AnZ",{}]""")

        * - rw(Hierarchy.B(1): Hierarchy.A, """["upickle.Hierarchy.B", {"i":1}]""")
        * - rw(C("a", "b"): A, """["upickle.Hierarchy.C",{"s1":"a","s2":"b"}]""")
      }

      'deep{
        import DeepHierarchy._
        implicit def Arw: RW[A] = upickle.legacy.macroRW
        implicit def Brw: RW[B] = upickle.legacy.macroRW
        implicit def Crw: RW[C] = upickle.legacy.macroRW
        implicit def AnQrw: RW[AnQ] = upickle.legacy.macroRW
        implicit def Qrw: RW[Q] = upickle.legacy.macroRW
        implicit def Drw: RW[D] = upickle.legacy.macroRW
        implicit def Erw: RW[E] = upickle.legacy.macroRW
        implicit def Frw: RW[F] = upickle.legacy.macroRW
        * - rw(B(1), """["upickle.DeepHierarchy.B",{"i":1}]""")
        * - rw(B(1): A, """["upickle.DeepHierarchy.B",{"i":1}]""")
        * - rw(AnQ(1): Q, """["upickle.DeepHierarchy.AnQ",{"i":1}]""")
        * - rw(AnQ(1), """["upickle.DeepHierarchy.AnQ",{"i":1}]""")

        * - rw(F(AnQ(1)), """["upickle.DeepHierarchy.F",{"q":["upickle.DeepHierarchy.AnQ",{"i":1}]}]""")
        * - rw(F(AnQ(2)): A, """["upickle.DeepHierarchy.F",{"q":["upickle.DeepHierarchy.AnQ",{"i":2}]}]""")
        * - rw(F(AnQ(3)): C, """["upickle.DeepHierarchy.F",{"q":["upickle.DeepHierarchy.AnQ",{"i":3}]}]""")
        * - rw(D("1"), """["upickle.DeepHierarchy.D",{"s":"1"}]""")
        * - rw(D("1"): C, """["upickle.DeepHierarchy.D",{"s":"1"}]""")
        * - rw(D("1"): A, """["upickle.DeepHierarchy.D",{"s":"1"}]""")
        * - rw(E(true), """["upickle.DeepHierarchy.E",{"b":true}]""")
        * - rw(E(true): C, """["upickle.DeepHierarchy.E",{"b":true}]""")
        * - rw(E(true): A, """["upickle.DeepHierarchy.E",{"b":true}]""")
      }
    }
    'singleton {
      import Singletons._

      implicit def AArw: RW[AA] = legacy.macroRW
      rw(BB, """["upickle.Singletons.BB",{}]""")
      rw(CC, """["upickle.Singletons.CC",{}]""")
      rw(BB: AA, """["upickle.Singletons.BB",{}]""")
      rw(CC: AA, """["upickle.Singletons.CC",{}]""")
    }
    'robustnessAgainstVaryingSchemas {
      'renameKeysViaAnnotations {
        import Annotated._
        implicit def Arw: RW[A] = upickle.legacy.macroRW
        implicit def Brw: RW[B] = upickle.legacy.macroRW
        implicit def Crw: RW[C] = upickle.legacy.macroRW
        * - rw(B(1), """["0", {"omg":1}]""")
        * - rw(C("a", "b"), """["1", {"lol":"a","wtf":"b"}]""")

        * - rw(B(1): A, """["0", {"omg":1}]""")
        * - rw(C("a", "b"): A, """["1", {"lol":"a","wtf":"b"}]""")
      }
      'useDefaults {
        // Ignore the values which match the default when writing and
        // substitute in defaults when reading if the key is missing
        import Defaults._
        implicit def Arw: RW[ADTa] = upickle.legacy.macroRW
        implicit def Brw: RW[ADTb] = upickle.legacy.macroRW
        implicit def Crw: RW[ADTc] = upickle.legacy.macroRW
        * - rw(ADTa(), "{}")
        * - rw(ADTa(321), """{"i":321}""")
        * - rw(ADTb(s = "123"), """{"s":"123"}""")
        * - rw(ADTb(i = 234, s = "567"), """{"i":234,"s":"567"}""")
        * - rw(ADTc(s = "123"), """{"s":"123"}""")
        * - rw(ADTc(i = 234, s = "567"), """{"i":234,"s":"567"}""")
        * - rw(ADTc(t = (12.3, 45.6), s = "789"), """{"s":"789","t":[12.3,45.6]}""")
        * - rw(ADTc(t = (12.3, 45.6), s = "789", i = 31337), """{"i":31337,"s":"789","t":[12.3,45.6]}""")
      }
      'ignoreExtraFieldsWhenDeserializing {
        import ADTs._
        implicit def ADTarw: RW[ADTs.ADTa] = upickle.legacy.macroRW
        implicit def ADTbrw: RW[ADTs.ADTb] = upickle.legacy.macroRW

        val r1 = upickle.legacy.read[ADTa]( """{"i":123, "j":false, "k":"haha"}""")
        assert(r1 == ADTa(123))
        val r2 = upickle.legacy.read[ADTb]( """{"i":123, "j":false, "k":"haha", "s":"kk", "l":true, "z":[1, 2, 3]}""")
        assert(r2 == ADTb(123, "kk"))
      }
    }

    'generics{
      import GenericADTs._
      * - {
        val pref1 = "upickle.GenericADTs.Delta"
        val D1 = Delta
        implicit def D1rw[A: R: W, B: R: W]: RW[D1[A, B]] = upickle.legacy.macroRW
        implicit def Insertrw[A: R: W, B: R: W]: RW[D1.Insert[A, B]] = upickle.legacy.macroRW
        implicit def Removerw[A: R: W]: RW[D1.Remove[A]] = upickle.legacy.macroRW
        implicit def Clearrw: RW[D1.Clear] = upickle.legacy.macroRW
        type D1[+A, +B] = Delta[A, B]
        rw(D1.Insert(1, 1), s"""["$pref1.Insert",{"key":1,"value":1}]""")
        rw(D1.Insert(1, 1): D1[Int, Int], s"""["$pref1.Insert",{"key":1,"value":1}]""")
        rw(D1.Remove(1), s"""["$pref1.Remove",{"key":1}]""")
        rw(D1.Remove(1): D1[Int, Int], s"""["$pref1.Remove",{"key":1}]""")
        rw(D1.Clear(), s"""["$pref1.Clear",{}]""")
        rw(D1.Clear(): D1[Int, Int], s"""["$pref1.Clear",{}]""")
      }
      * - {
        val pref2 = "upickle.GenericADTs.DeltaInvariant"
        val D2 = DeltaInvariant
        type D2[A, B] = DeltaInvariant[A, B]
        implicit def D2rw[A: R: W, B: R: W]: RW[D2[A, B]] = upickle.legacy.macroRW
        implicit def Insertrw[A: R: W, B: R: W]: RW[D2.Insert[A, B]] = upickle.legacy.macroRW
        implicit def Removerw[A: R: W, B]: RW[D2.Remove[A, B]] = upickle.legacy.macroRW
        implicit def Clearrw[A, B]: RW[D2.Clear[A, B]] = upickle.legacy.macroRW
        rw(D2.Insert(1, 1), s"""["$pref2.Insert",{"key":1,"value":1}]""")
        rw(D2.Insert(1, 1): D2[Int, Int], s"""["$pref2.Insert",{"key":1,"value":1}]""")
        rw(D2.Remove(1), s"""["$pref2.Remove",{"key":1}]""")
        rw(D2.Remove(1): D2[Int, Int], s"""["$pref2.Remove",{"key":1}]""")
        rw(D2.Clear(), s"""["$pref2.Clear",{}]""")
        rw(D2.Clear(): D2[Int, Int], s"""["$pref2.Clear",{}]""")
      }
    }
    'recursiveDataTypes{
      import Recursive._
      implicit def IntTreerw: RW[IntTree] = upickle.legacy.macroRW
      implicit def SingleNoderw: RW[SingleNode] = upickle.legacy.macroRW

      implicit def SingleTreerw: RW[SingleTree] = upickle.legacy.macroRW

      implicit def Noderw: RW[Node] = upickle.legacy.macroRW


      implicit def LLrw: RW[LL] = upickle.legacy.macroRW
      rw(
        IntTree(123, List(IntTree(456, Nil), IntTree(789, Nil))),
        """{"value":123,"children":[{"value":456,"children":[]},{"value":789,"children":[]}]}"""
      )
      rw(
        SingleNode(123, List(SingleNode(456, Nil), SingleNode(789, Nil))),
        """["upickle.Recursive.SingleNode",{"value":123,"children":[["upickle.Recursive.SingleNode",{"value":456,"children":[]}],["upickle.Recursive.SingleNode",{"value":789,"children":[]}]]}]"""
      )
      rw(
        SingleNode(123, List(SingleNode(456, Nil), SingleNode(789, Nil))): SingleTree,
        """["upickle.Recursive.SingleNode",{"value":123,"children":[["upickle.Recursive.SingleNode",{"value":456,"children":[]}],["upickle.Recursive.SingleNode",{"value":789,"children":[]}]]}]"""
      )
      rw(End: LL, """["upickle.Recursive.End",{}]""")
      rw(Node(3, End): LL, """["upickle.Recursive.Node",{"c":3,"next":["upickle.Recursive.End",{}]}]""")
      rw(Node(6, Node(3, End)), """["upickle.Recursive.Node",{"c":6,"next":["upickle.Recursive.Node",{"c":3,"next":["upickle.Recursive.End",{}]}]}]""")

    }
    'varargs{
      implicit def IntTreerw: RW[Varargs.Sentence] = upickle.legacy.macroRW
      rw(Varargs.Sentence("a", "b", "c"), """{"a":"a","bs":["b","c"]}""")
      rw(Varargs.Sentence("a"), """{"a":"a","bs":[]}""")
    }


    'issues - {
      'issue95 {
        implicit def rw1: RW[C1] = legacy.macroRW
        implicit def rw2: RW[C2] = legacy.macroRW
        implicit def rw3: RW[GeoCoding2] = legacy.macroRW
        implicit def rw4: RW[Result2] = legacy.macroRW
        rw(
          Tuple1(List(C1("hello", List("world")))),
          """[[{"name": "hello", "types": ["world"]}]]"""
        )
        rw(
          C2(List(C1("hello", List("world")))),
          """{"results": [{"name": "hello", "types": ["world"]}]}"""
        )

        rw(
          GeoCoding2(List(Result2("a", "b", List("c"))), "d"),
          """{"results": [{"name": "a", "whatever": "b", "types": ["c"]}], "status": "d"}"""
        )
      }
      'scalatex - {
        implicit def rw1: RW[Ast] = legacy.macroRW
        implicit def rw2: RW[Ast.Block] = legacy.macroRW
        implicit def rw3: RW[Ast.Block.Sub] = legacy.macroRW
        implicit def rw4: RW[Ast.Block.Text] = legacy.macroRW
        implicit def rw5: RW[Ast.Block.For] = legacy.macroRW
        implicit def rw6: RW[Ast.Block.IfElse] = legacy.macroRW
        implicit def rw7: RW[Ast.Header] = legacy.macroRW
        implicit def rw8: RW[Ast.Chain] = legacy.macroRW
        implicit def rw9: RW[Ast.Chain.Sub] = legacy.macroRW
        implicit def rw10: RW[Ast.Chain.Prop] = legacy.macroRW
        implicit def rw11: RW[Ast.Chain.TypeArgs] = legacy.macroRW
        implicit def rw12: RW[Ast.Chain.Args] = legacy.macroRW
        val block = Ast.Block(1, Seq(Ast.Block.Text(2, "hello")))
        val blockText = """[
          "upickle.Ast.Block",
          {
            "offset":1,
            "parts":[
              [
                "upickle.Ast.Block.Text",
                {
                  "offset":2,
                  "txt":"hello"
                }
              ]
            ]
          }
        ]"""
        rw(block: Ast, blockText)
        rw(block: Ast.Block, blockText)
        rw(block: Ast.Block.Sub, blockText)
        rw(block: Ast.Chain.Sub, blockText)

        val header = Ast.Header(0, "Hello", block)
        val headerText = s"""[
          "upickle.Ast.Header",
          {
            "offset": 0,
            "front": "Hello",
            "block": $blockText
          }
        ]"""
        rw(header: Ast, headerText)
        rw(header: Ast.Header, headerText)
        rw(header: Ast.Block.Sub, headerText)
        rw(header: Ast.Chain.Sub, headerText)
      }
//      'companionImplicitPickedUp{
//        assert(implicitly[upickle.default.Reader[TypedFoo]] eq TypedFoo.readWriter)
//        assert(implicitly[upickle.default.Writer[TypedFoo]] eq TypedFoo.readWriter)
//        assert(implicitly[upickle.default.ReadWriter[TypedFoo]] eq TypedFoo.readWriter)
//      }
  //    'companionImplicitWorks{
  //
  //      rw(TypedFoo.Bar(1): TypedFoo, """{"$type": "upickle.TypedFoo.Bar", "i": 1}""")
  //      rw(TypedFoo.Baz("lol"): TypedFoo, """{"$type": "upickle.TypedFoo.Baz", "s": "lol"}""")
  //      rw(TypedFoo.Quz(true): TypedFoo, """{"$type": "upickle.TypedFoo.Quz", "b": true}""")
  //    }
    }
    'jsonInCaseClass - {

      implicit def arw: RW[CaseClassWithJson] = upickle.legacy.macroRW
      rw(new CaseClassWithJson(Js.Num(7)), """{"json":7}""")
      rw(new CaseClassWithJson(Js.Arr(Js.Num(7), Js.Str("lol"))), """{"json":[7,"lol"]}""")
    }
    'traitFromOtherPackage - {
      implicit val BaseRW: RW[subpackage.Base] = upickle.legacy.macroRW
      implicit val WrapperRW: RW[subpackage.Wrapper] = upickle.legacy.macroRW
      upickle.legacy.write(subpackage.Wrapper(subpackage.Base.Child))
    }
//    'performance{
//      import Generic.ADT
//      import Hierarchy._
//      import Recursive._
//      import Defaults._
//      import ADTs.ADT0
//      type Data = ADT[Seq[(Int, Int)], String, A, LL, ADTc, ADT0]
//      val data: Data = ADT(
//        Vector((1, 2), (3, 4), (4, 5), (6, 7), (8, 9), (10, 11), (12, 13)),
//        """
//          |I am cow, hear me moo
//          |I weigh twice as much as you
//          |And I look good on the barbecueeeee
//        """.stripMargin,
//        C("lol i am a noob", "haha you are a noob"): A,
//        Node(-11, Node(-22, Node(-33, Node(-44, End)))): LL,
//        ADTc(i = 1234567890, s = "i am a strange loop"),
//        ADT0()
//      )
//      implicit def rw1: RW[Data] = upickle.legacy.macroRW
//      implicit def rw2: RW[A] = upickle.legacy.macroRW
//      implicit def rw3: RW[B] = upickle.legacy.macroRW
//      implicit def rw4: RW[C] = upickle.legacy.macroRW
//      implicit def rw5: RW[LL] = upickle.legacy.macroRW
//      implicit def rw6: RW[Node] = upickle.legacy.macroRW
//      implicit def rw7: RW[End.type] = upickle.legacy.macroRW
//      implicit def rw8: RW[ADTc] = upickle.legacy.macroRW
//      implicit def rw9: RW[ADT0] = upickle.legacy.macroRW
//
//      // Some arbitrary data that represents a mix of all the different
//      // ways things can be pickled and unpickled
//
//      val stringified = upickle.legacy.write(data)
//      val r1 = upickle.legacy.read[Data](stringified)
//      assert(data == r1)
//      val rewritten = upickle.legacy.write(upickle.legacy.read[Data](stringified))
//      assert(stringified == rewritten)
//
//      'read{
//        var n = 0
//        val start = System.currentTimeMillis()
//        while(System.currentTimeMillis() < start + 50000){
//          upickle.legacy.read[Data](stringified)
//          n += 1
//        }
//        n
//      }
//      'write{
//        var n = 0
//        val start = System.currentTimeMillis()
//        while(System.currentTimeMillis() < start + 50000){
//          upickle.legacy.write(data)
//          n += 1
//        }
//        n
//      }
//    }
  }
}
