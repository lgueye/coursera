package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("isStanding level 1") {
    new Level1 {
      assert(startBlock.isStanding)
      assert(!startBlock.left.isStanding)
      assert(!startBlock.up.isStanding)
      assert(!startBlock.right.isStanding)
      assert(!startBlock.down.isStanding)
    }
  }
  
  test("isLegal level 1, start position") {
    new Level1 {
      assert(startBlock.isLegal)
      assert(!startBlock.left.isLegal)
      assert(!startBlock.up.isLegal)
      assert(startBlock.right.isLegal)
      assert(startBlock.down.isLegal)
    }
  }

  test("neighbors level 1, start position") {
    new Level1 {
      assert(startBlock.isLegal)
      assert(startBlock.neighbors.contains(startBlock.left, Left))
      assert(startBlock.neighbors.contains(startBlock.right, Right))
      assert(startBlock.neighbors.contains(startBlock.up, Up))
      assert(startBlock.neighbors.contains(startBlock.down, Down))
    }
  }

  test("legal neighbors level 1, start position") {
    new Level1 {
      assert(startBlock.isLegal)
      assert(!startBlock.legalNeighbors.contains(startBlock.left, Left))
      assert(startBlock.legalNeighbors.contains(startBlock.right, Right))
      assert(!startBlock.legalNeighbors.contains(startBlock.up, Up))
      assert(startBlock.legalNeighbors.contains(startBlock.down, Down))
    }
  }
  
  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
