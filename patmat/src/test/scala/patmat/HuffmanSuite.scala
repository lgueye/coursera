package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    val result = times(string2Chars("aabbbcdddddd"))
    assert (result.exists(tuple => tuple == ('a',2)))
    assert (result.exists(tuple => tuple == ('b',3)))
    assert (result.exists(tuple => tuple == ('c',1)))
    assert (result.exists(tuple => tuple == ('d',6)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton list") {
    assert (!singleton(Nil))
    assert (singleton( List(Leaf('a',5)) ))
    assert (!singleton( List(Leaf('a',5), Leaf('a',5)) ))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of some leaf list 2") {
    val leaflist = List(Leaf('e', 1), Leaf('v', 10), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Leaf('t',2), Leaf('x',4), Fork(Leaf('e',1),Leaf('v',10),List('e', 'v'),11)))
  }
  
  test("combine of some leaf list 3") {
    val leaflist = List(Leaf('b', 2), Leaf('c', 3), Leaf('d', 4), Leaf('f', 6), Leaf('g', 7))
    assert(combine(leaflist) === List(Leaf('d',4), Fork(Leaf('b', 2), Leaf('c', 3), List('b', 'c'), 5), Leaf('f', 6), Leaf('g', 7)))
  }

  test("until should reduce to a singleton") {
    val leaflist = List(Leaf('b', 2), Leaf('c', 3), Leaf('d', 4), Leaf('f', 6), Leaf('g', 7))
    assert(singleton(until(singleton, combine)(leaflist)))
  }
  
  
  test("encode t1 should succeed") {
    new TestTrees {
      assert(List(0,1) === encode(t1)("ab".toList))
    }
  }
  
  test("encode t2 should succeed") {
    new TestTrees {
      assert(List(1, 0, 0, 0, 1) === encode(t2)("dab".toList))
    }
  }

  test("decode t2 should succeed") {
    new TestTrees {
      assert("dab".toList === decode(t2,List(1, 0, 0, 0, 1)))
    }
  }
  
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  
  test("codeBits should succeed") {
    val bits = List(1,0,1,0)
    val table = List(('a',List(0,1)),('b', List(0,0,1)),('c',bits),('d', List(1,1,1,0)))
	assert(bits === codeBits(table)('c'))
  }
  
  test("convert should succeed") {
    new TestTrees {
      val expected = List(('a', List(0,0)), ('b', List(0,1)), ('d',List(1)))
	  assert(expected === convert(t2))
    }
  }
 
  test("quickEncode t2 should succeed") {
    new TestTrees {
      assert(List(1, 0, 0, 0, 1) === quickEncode(t2)("dab".toList))
    }
  }
  
}
