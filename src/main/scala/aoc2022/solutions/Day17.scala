package aoc2022.solutions

import aoc2022.utils.*

object Day17 {
  val sc = scannerFromResource("/day17.txt")
  val lines = scannerToLines(sc)

  val pushes = lines.head.trim.toCharArray

  case class LongPoint(x: Long, y: Long) {
    override def toString: String = s"($x,$y)"
  }
  
  val horBar = Rock(Seq(LongPoint(0, 0), LongPoint(1, 0), LongPoint(2, 0), LongPoint(3, 0)), 4, 1)
  val cross = Rock(Seq(LongPoint(1, 0), LongPoint(0, -1), LongPoint(1, -1), LongPoint(2, -1), LongPoint(1, -2)), 3, 3)
  val hook = Rock(Seq(LongPoint(2, 0), LongPoint(2, -1), LongPoint(0, -2), LongPoint(1, -2), LongPoint(2, -2)), 3, 3)
  val verBar = Rock(Seq(LongPoint(0, 0), LongPoint(0, -1), LongPoint(0, -2), LongPoint(0, -3)), 1, 4)
  val block = Rock(Seq(LongPoint(0, 0), LongPoint(1, 0), LongPoint(0, -1), LongPoint(1, -1)), 2, 2)

  val rocks: Array[Rock] = List[Rock](horBar, cross, hook, verBar, block).toArray

  case class Rock(parts: Seq[LongPoint], width: Int, height: Int, var x: Int = 0, var y: Long = 0) {
    def realParts = parts.map { p =>
      p.copy(x = p.x + x, y = p.y + y)
    }
  }

  def blockedDownwards(rock: Rock): Boolean =
    (rock.y - rock.height) == 0 || {
      // check for contact with blocked rocks.
      val candidates = droppedRocks.takeWhile(_.y >= rock.y - rock.height )
      candidates.exists { cr =>
        // blocked if any of the parts of rock has same x and is blocked by any of the parts of cr
        rock.realParts.exists {  rp =>
          cr.realParts.exists {  crp =>
            crp.x == rp.x && crp.y == rp.y - 1 // 1 lower and same x means blocking
          }
        }
      }
    }

  // direction: left = -1, right = 1
  def blockedHorizontally(rock: Rock, direction: Int): Boolean =
    (direction < 0 && rock.x == 0) || (direction > 0 && rock.x + rock.width == 7) || {
      // check for contact with blocked rocks.
      val candidates = droppedRocks.takeWhile { dr => dr.y >= rock.y - rock.height }
      candidates.exists { cr =>
        // blocked if any of the parts of rock has same y and is blocked by any of the parts of cr
        rock.realParts.exists { rp =>
          cr.realParts.exists { crp =>
            crp.y == rp.y && crp.x == rp.x + direction
          }
        }
      }
    }
   

  def moveRock(push: Char, rock: Rock): Unit = {
    val direction = push match {
      case '<' => -1
      case '>' => +1
      case _ => sys.error("Unknown direction: " + push)
    }

    if !blockedHorizontally(rock, direction) then
      rock.x = rock.x + direction
  }

  var dropped = 0L
  var highestRockPos = 0L
  var pushPos = 0L
  var droppedRocks = scala.collection.mutable.ListBuffer[Rock]()
  
  while dropped < 2022 do {
    val rock = rocks((dropped % 5).toInt).copy()
    rock.y = highestRockPos + rock.height + 3 
    rock.x = 2

    var inRest = false
    while !inRest do {
      val push = pushes((pushPos % pushes.length).toInt)
      pushPos = pushPos + 1

      moveRock(push, rock)
      
      if !blockedDownwards(rock) then rock.y = rock.y - 1
      else {
        var insertIndex = 0
        while (insertIndex < droppedRocks.length && droppedRocks(insertIndex).y > rock.y) insertIndex = insertIndex + 1
        droppedRocks.insert(insertIndex, rock)  // keep list sorted by descending top y
        droppedRocks = droppedRocks.take(50)
        highestRockPos = droppedRocks.head.y
        inRest = true
      }
    }

    dropped = dropped + 1
    if rock.y >= highestRockPos then highestRockPos = rock.y
  }

  @main
  def day17Part1 = printSolution {
      highestRockPos   // 3168
  }

  @main
  def day17Part2 = printSolution {
    // At pushpos 290 (modulo), dropped 1750  we get a repeating position
    // height was 2731

    // 2nd time:
    // pushpos again 290
    // dropped 3450. So increased 1700
    // height: 5373. So added 2642

    // 3nd time:
    // pushpos again 290
    // dropped 5150, 1700 more
    // height: 8015. So cycle again 2642 so confirmed

    // we want to know:
    // how many drops until position repeats again
    // what does droppedRocks, highestRockPos look like

    // so: do 1750 drops. Gather rocks. Start height is 2731
    // then add ((1,000,000,000,000 - 1750) / 1700) * 2642 to height
    // then do leftover:
    // (1,000,000,000,000 - 1750) % 1700 = another 150
    // actually we can do 1750 + 150 and take the height from there: 2964 is the height
    // so solution = 2964 +  ((1,000,000,000,000 - 1750) / 1700) * 2642 =

    ((1000000000000L - 1750) / 1700) * 2642  + 2964
  }
}
