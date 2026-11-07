package org.lemon.advent.year2024

private object Day09:

  trait Segment
  case class Free(length: Int) extends Segment
  case class File(length: Int, id: Int) extends Segment

  def parse(input: String) = input
    .zipWithIndex
    .map((s, i) => if i % 2 == 0 then File(s.asDigit, i / 2) else Free(s.asDigit))

  def defragBlocks(segments: Seq[Segment]) =
    @annotation.tailrec
    def defrag(segments: Seq[Segment], reverse: Seq[File], result: Seq[File] = Seq()): Seq[File] =
      val toMove @ File(len, moveId) = reverse.head
      segments.head match
        case File(_, id) if id >= moveId => result :+ toMove
        case Free(0) | File(0, _) => defrag(segments.tail, reverse, result)
        case Free(n) if len <= n => // move whole file; free space may remain
          defrag(Free(n - len) +: segments.tail, reverse.tail, result :+ toMove)
        case Free(n) if len > n => // move part of file; no free space left
          defrag(segments.tail, File(len - n, moveId) +: reverse.tail, result :+ File(n, moveId))
        case file: File => // a file; just copy it forward
          defrag(segments.tail, reverse, result :+ file)

    defrag(segments, segments.reverse.collect { case f: File => f })

  def checksum(files: Seq[Segment]) =
    files.iterator
      .flatMap(_ match
        case File(len, id) => Iterator.fill(len)(id.toLong)
        case Free(len) => Iterator.fill(len)(0L)
      )
      .zipWithIndex
      .map(_ * _)
      .sum

  def part1(input: String) =
    val segments = parse(input)
    // this also works and shares part 2 code entirely, but it's like 100x slower
    // val blocks = segments.flatMap(_ match
    //   case File(len, id) => Seq.fill(len)(File(1, id))
    //   case Free(len) => Seq.fill(len)(Free(1))
    // )
    // val files = defragFiles(blocks)
    val files = defragBlocks(segments)
    checksum(files)

  def defragFiles(segments: Seq[Segment]) =
    @annotation.tailrec
    def defrag(segments: Seq[Segment], result: Seq[Segment] = Seq()): Seq[Segment] =
      segments match
        case Free(0) +: tail => defrag(tail, result)
        case Free(n) +: tail =>
          tail.lastIndexWhere { case File(len, id) if len <= n => true; case _ => false } match
            case -1 => defrag(tail, result :+ Free(n))
            case x =>
              val file = tail(x).asInstanceOf[File]
              defrag(Free(n - file.length) +: tail.updated(x, Free(file.length)), result :+ file)
        case File(0, _) +: tail => defrag(tail, result)
        case (file: File) +: tail => defrag(tail, result :+ file)
        case Nil => result

    defrag(segments)

  def part2(input: String) =
    val segments = parse(input)
    val files = defragFiles(segments)
    checksum(files)
