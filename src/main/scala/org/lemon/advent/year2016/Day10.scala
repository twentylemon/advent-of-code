package org.lemon.advent.year2016

private object Day10:

  enum Dest:
    case Bot(id: Int)
    case Output(id: Int)
  import Dest.*

  case class Robit(
      id: Int,
      has: Seq[Int] = Seq.empty,
      lowTo: Dest = null,
      highTo: Dest = null,
      compared: Seq[(Int, Int)] = Vector.empty,
  )

  def parse(input: String) =
    def dest(typ: String, id: String) = if typ == "bot" then Bot(id.toInt) else Output(id.toInt)
    input.linesIterator.foldLeft(Map.empty[Int, Robit])((robits, line) =>
      line match
        case s"value $n goes to bot $b" => robits.updatedWith(b.toInt) {
            case Some(robit) => Some(robit.copy(has = robit.has :+ n.toInt))
            case None => Some(Robit(id = b.toInt, has = Seq(n.toInt)))
          }
        case s"bot $b gives low to $lt $l and high to $ht $h" => robits.updatedWith(b.toInt) {
            case Some(robit) => Some(robit.copy(lowTo = dest(lt, l), highTo = dest(ht, h)))
            case None => Some(Robit(id = b.toInt, lowTo = dest(lt, l), highTo = dest(ht, h)))
          }
    )

  def step(robits: Map[Int, Robit], outputs: Map[Int, Seq[Int]]) =
    def give(ro: (Map[Int, Robit], Map[Int, Seq[Int]]), dest: Dest, value: Int) =
      dest match
        case Bot(id) =>
          ro._1.updatedWith(id)(_.map(r => r.copy(has = r.has :+ value))) -> ro._2
        case Output(id) =>
          ro._1 -> ro._2.updatedWith(id)(v => Some(v.getOrElse(Seq.empty) :+ value))

    robits.find((_, r) => r.has.size == 2).map((id, giver) =>
      val low = giver.has.min
      val high = giver.has.max

      val clear = robits.updated(id, giver.copy(has = Seq.empty, compared = giver.compared :+ (low, high)))
      give(give(clear -> outputs, giver.lowTo, low), giver.highTo, high)
    )

  def part1(input: String, compares: (Int, Int) = (17, 61)) =
    val robits = parse(input)
    Iterator.unfold(robits -> Map.empty[Int, Seq[Int]])(step.tupled(_).map(x => x -> x))
      .flatMap((r, _) => r.values.find(_.compared.contains(compares)))
      .next.id

  def part2(input: String) =
    val robits = parse(input)
    val (_, out) = LazyList.unfold(robits -> Map.empty[Int, Seq[Int]])(step.tupled(_).map(x => x -> x)).last
    (0 to 2).map(out(_).head).product
