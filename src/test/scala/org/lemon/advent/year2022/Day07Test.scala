package org.lemon.advent.year2022

import scala.collection.mutable
import org.lemon.UnitTest
import scala.util.Using
import scala.io.Source

class Day07Test extends UnitTest {

    sealed trait File {
        def size: Int
        def name: String
    }
    case class Directory(name: String, parent: Directory, contents: mutable.Set[File] = mutable.Set()) extends File {
        def size = contents.map(_.size).sum
    }
    case class Obj(name: String, size: Int) extends File

    def buildTree(lines: Iterator[String]) =
        val root = Directory("/", parent=null)
        var pwd = root
        lines.foreach(_ match
            case "$ cd .." => pwd = pwd.parent
            case "$ cd /" => pwd = root
            case s"$$ cd $dir" => pwd = pwd.contents.filter(_.isInstanceOf[Directory]).map(_.asInstanceOf[Directory]).find(_.name == dir).get
            case "$ ls" => "nothing to do"
            case s"dir $dir" => pwd.contents.add(Directory(dir, pwd))
            case s"$size $name" => pwd.contents.add(Obj(name=name, size=size.toInt))
        )
        root

    def iterator(tree: File): Iterator[File] = tree match
        case obj: Obj => Iterator(obj)
        case dir: Directory => Iterator(dir) ++ dir.contents.flatMap(iterator)

    def part1(in: String) =
        val root = buildTree(in.linesIterator)
        iterator(root)
            .filter(_.isInstanceOf[Directory])
            .map(_.size)
            .filter(_ <= 100000)
            .sum

    def part2(in: String) =
        val total = 70000000
        val required = 30000000
        val root = buildTree(in.linesIterator)
        val unused = 70000000 - root.size
        val needToIce = required - unused
        iterator(root)
            .filter(_.isInstanceOf[Directory])
            .map(_.size)
            .filter(_ >= needToIce)
            .min


    test("part 1 example") {
        val in = """
            |$ cd /
            |$ ls
            |dir a
            |14848514 b.txt
            |8504156 c.dat
            |dir d
            |$ cd a
            |$ ls
            |dir e
            |29116 f
            |2557 g
            |62596 h.lst
            |$ cd e
            |$ ls
            |584 i
            |$ cd ..
            |$ cd ..
            |$ cd d
            |$ ls
            |4060174 j
            |8033020 d.log
            |5626152 d.ext
            |7214296 k""".stripMargin.strip
        
        buildTree(in.linesIterator).size shouldBe 48381165
        part1(in) shouldBe 95437
    }

    test("part 1") {
        Using.resource(Source.fromResource("year2022/day07.txt"))(source =>
            part1(source.mkString) shouldBe 1770595
        )
    }

    test("part 2 example") {
        val in = """
            |$ cd /
            |$ ls
            |dir a
            |14848514 b.txt
            |8504156 c.dat
            |dir d
            |$ cd a
            |$ ls
            |dir e
            |29116 f
            |2557 g
            |62596 h.lst
            |$ cd e
            |$ ls
            |584 i
            |$ cd ..
            |$ cd ..
            |$ cd d
            |$ ls
            |4060174 j
            |8033020 d.log
            |5626152 d.ext
            |7214296 k""".stripMargin.strip
        
        part2(in) shouldBe 24933642
    }

    test("part 2") {
        Using.resource(Source.fromResource("year2022/day07.txt"))(source =>
            part2(source.mkString) shouldBe 2195372
        )
    }
  
}
