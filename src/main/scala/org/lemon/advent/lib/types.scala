package org.lemon.advent.lib

/** Evidence that a type parameter `N` is `D` when nothing else determines it. */
final class DefaultsTo[N, D] private ()

object DefaultsTo:
  given same[D]: DefaultsTo[D, D] = DefaultsTo()
  given other[N, D]: DefaultsTo[N, D] = DefaultsTo()
