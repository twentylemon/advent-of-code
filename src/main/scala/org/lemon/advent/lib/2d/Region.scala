package org.lemon.advent.lib.`2d`

object Region:
  given Conversion[Region, Iterator[Coord]] = _.coords.iterator
  given Conversion[Region, Set[Coord]] = _.coords

/** An arbitrary region of continuous space on a 2d grid.
  * @param coords the set of coordinates that make up the region
  */
case class Region(coords: Set[Coord]):
  def area = coords.size

  /** The perimeter of a region is the set of all coords that are on the boarder of the region.
    * Coords which are diagonally adjacent are included in the perimeter, such that it forms
    * a "continuous" line around the region.
    * @return set of coords that are one away from the region
    */
  def perimeter: Set[Coord] = coords.filter(_.surrounding.exists(!coords.contains(_)))

  def perimeterLength = perimeter.iterator.map(_.adjacent.count(!coords.contains(_))).sum

  def sides =
    def in(coord: Coord) = coords(coord)
    def out(coord: Coord) = !coords(coord)
    def sides(coord: Coord) =
      // coord is adjacent to the outside, each contributes 1 side
      val topLeft = out(coord.up) && out(coord.left)
      val topRight = out(coord.up) && out(coord.right)
      val botLeft = out(coord.down) && out(coord.left)
      val botRight = out(coord.down) && out(coord.right)

      // coord is diagonal to the outside, the shape has turned a corner
      val diagUpLeft = in(coord.up) && in(coord.left) && out(coord.up.left)
      val diagUpRight = in(coord.up) && in(coord.right) && out(coord.up.right)
      val diagBotLeft = in(coord.down) && in(coord.left) && out(coord.down.left)
      val diagBotRight = in(coord.down) && in(coord.right) && out(coord.down.right)

      Seq(topLeft, topRight, botLeft, botRight, diagUpLeft, diagUpRight, diagBotLeft, diagBotRight)
        .count(identity)

    perimeter.iterator.map(sides).sum

  def enclosing = Area(coords)
