package org.lemon.advent.year2015

import io.circe.*
import io.circe.parser.*

private object Day12:

  def sum(json: Json, filter: JsonObject => Boolean): Int = json.fold(
    jsonNull = 0,
    jsonBoolean = _ => 0,
    jsonNumber = _.toInt.getOrElse(0),
    jsonString = _ => 0,
    jsonArray = _.map(sum(_, filter)).sum,
    jsonObject = o => if filter(o) then o.values.map(sum(_, filter)).sum else 0
  )

  def part1(input: String) =
    val json = parse(input).getOrElse(Json.Null)
    sum(json, _ => true)

  def part2(input: String) =
    val json = parse(input).getOrElse(Json.Null)
    sum(json, !_.values.exists(j => j.isString && j.asString.get == "red"))
