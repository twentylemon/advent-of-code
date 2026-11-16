# advent-of-code

Scala 3 (sbt 2) solutions to https://adventofcode.com, plus a shared library in `lib/`. There's one Python test (2022 day 19). The repo is public.

## Solving puzzles: the ideas are the user's

The user comes up with every idea for solving a puzzle. Claude can write the code, but never the idea.

- **Never solve a puzzle or suggest how to.** That includes approaches, algorithms, data structures, key insights, hints and "have you considered X?" questions. Don't work out a solution on the side either.
- **Wait for the user to propose an approach.** Then help prove or disprove it with probing questions: edge cases, counterexamples, how it scales to the real input size, what the puzzle text guarantees. A question must not hide an approach.
- **Writing code is fine** when it implements an approach the user described (parsing, the solution, tests). If the code turns up a gap in the idea, ask about the gap. Don't fill it with your own idea.
- **Bugs before the day is solved: questions only.** For a wrong answer, a crash or a hang, ask questions that lead the user to the bug ("what does `parse` return for the last line?"). Don't point at the line and don't fix it.
- **A day is solved** when both parts have example tests and real-input tests that pass with the accepted answers. After that, suggest cleanups, lib adoption, perf work or a better approach freely. For perf work on an existing solution, see "Performance" below.

## Layout

- `src/main/scala/org/lemon/advent/yearYYYY/DayNN.scala`: `private object DayNN` with `parse`, `part1` and `part2`, each taking the input `String`. Solutions `import org.lemon.advent.lib.*`.
- `src/test/scala/.../yearYYYY/DayNNTest.scala` extends `UnitTest` (`AnyFunSuite` with `Matchers` and `Checkers`, at least 1000 successful property cases). Examples go inline with `stripMargin`. Real inputs are read with `read(file(year)(day))`.
- Inputs are in `src/main/resources/yearYYYY/dayNN.txt`, encrypted with git-crypt. Never copy them anywhere they could be committed decrypted, and never send them to an external service.
- `sbt "run 2025 1"` fetches a day's input and scaffolds its solution and test. It needs the gitignored `session` cookie file.
- `lib/`: `collections`, `range` (`Interval`, `Diet`), `geometry` (`Point`/`Coord`, `Point3`, `Rect`/`Area`, `Cuboid`, `Direction`, `Orientation`), `graph` (search, fill, cliques, `DisjointSet`, compression), `math`, `memoize`, `cycle`, `expressions`, `strings`, `ortools` (a MIP wrapper), and the `bench.scala` JMH stub.

## Commands

- Test one class: `sbt "testOnly org.lemon.advent.year2025.Day05Test"`. A package: `sbt "testOnly org.lemon.advent.lib.*"`.
- sbt 2 behaviour: `sbt test` is incremental (`testFull` forces a full run), and `sbt a b` only runs `a`, so write `sbt "a; b"`.
- Format: `sbt "scalafmtAll; scalafmtSbt"`. CI runs `scalafmtCheckAll; scalafmtSbtCheck`. For one file: `Test/scalafmtOnly <file>`.
- Python: `black --check .` and `pytest`, from `venv`.
- Local only, too heavy for CI: `sbt libCoverage` (scoverage over `lib`) and `sbt stryker` (mutation testing over `lib`).
- JMH: `sbt "Jmh/run -i 5 -wi 5 -r 1 -w 1 -f 2 -t 1 <bench class>"`.
- CI runs `testOnly` per package on every push. `gh run view <id> --log` has per-test durations.

## Workflow

- One PR per work item, each on its own `twentylemon/<slug>` branch off the current `main`.
- For a larger effort, open a tracking issue with **Work items**, **Dropped**, **Decisions and conventions** and **Status** sections (see #242, #257). Keep it current so the work can be picked up from the issue alone.
- Commit messages use gitmoji and a lowercase imperative summary, with identifiers in backticks: ``:zap: walk the smaller diet in `Diet.intersect` ``. In use: `:sparkles:` (new API), `:bug:`, `:zap:` (perf), `:recycle:`, `:art:` (style), `:white_check_mark:` (tests).
- Explanations go in commit messages and PR bodies, not in the source.
- Never commit temporary benchmark files.

## Lib API

- Add API only with a real caller, ideally adopted in the same PR. To find a method's callers, mark it `@deprecated` and run `sbt 'set scalacOptions += "-deprecation"; Test/compile'`, then shut the server down so the setting doesn't stick.
- Keep the API fairly small, and add things when a puzzle needs them.
- Collection-like types follow `Iterable`/`Seq` naming. Prefer overriding a member with a narrower return type (`Interval.span`) to inventing a new name. Set operations whose result isn't an interval (union, diff) belong to `Diet`.
- Conversions behave like the stdlib's numeric `toX`: they truncate silently and don't validate.
- A behaviour change needs a caller that actually sees the difference. Weigh that against the test churn (#243 was closed on these grounds). A wrong result is a bug worth fixing. Throwing a different exception class than the stdlib would isn't worth a PR.
- Empty `Interval`/`Rect`/`Cuboid` values keep their bounds, so there are many of them and `==` compares bounds. Check emptiness with `isEmpty`.

## Code style

- No comments that restate the code: use clear names instead (`val (big, small)`, `roomBelow`). Public lib methods get scaladoc in the file's existing style when it says something the signature doesn't.
- Keep descriptive names even where `-Wunused` flags them (`(k, v) =>`, named intermediates, debug printers). Don't swap them for `_` or delete them.
- Keep `extends X(` on the definition line. Shorten long lines with a `val`, not a helper.
- Public lib extensions declare their result types.
- scalafmt, `maxColumn = 120`.

## Tests

- Lib tests are scalacheck properties that state real properties (containment, ordering, round-trips, sizes). They don't re-implement the function. Comparing against a stdlib oracle (`sliding(n)`, `combinations`, `Range`) is fine. Restating the same spec in another form isn't a new test.
- Use the most generic generator that fits. Avoid `==>` preconditions and arbitrary bounds. When one input depends on another, generate the shape first and derive the amount from it. Use `Gen.chooseNum(Int.MinValue, -1)`, not `Gen.negNum`, which is sized and stays within -100..-1.
- Cover the whole `Int` range. Most properties are identities that hold under overflow. Compare inequalities in `BigInt`.
- Generator style: unrelated inputs go in an inline `forAll(arbitrary[...], ...)`, dependent inputs in a `val gen = for ... yield`, and implicit-arbitrary-only properties are written `check((xs: T) => ...)`.
- Don't test aliases (`--` vs `diff`). Don't step through an `Interval` value by value unless you're comparing it with a `Range`.
- Prove a property set isn't vacuous with a quick mutant: break the code, run the tests, restore it.
- Bug fixes: write the tests first. They must fail on the unfixed code.
- Stateful types (`Diet`, `MinQueue`, `DisjointSet`, `ExpressionContext`) use scalacheck `Commands`.
- `ortools` tests are example toy problems, not properties.

## Performance

- Perf work on a solution keeps its algorithm and search order. Swapping data structures or idioms is fine: an array for a map, a `BitSet` for a `Set`, a `groupMapReduce`. If the real win needs a different algorithm, describe it and ask.
- A lib perf change lands only with JMH evidence:
  - Put a temporary, uncommitted `lib/<Name>Bench.scala` next to the code, with `baseline*` copies of the current code beside the candidate so both run in the same JMH run.
  - It lands only if it's faster on its target case with a non-overlapping 99.9% CI, and no other param case is measurably slower.
  - Weigh regressions in absolute time. A few µs lost at tiny n is fine, unless a hot caller runs that case many times.
  - A microbenchmark win also has to show up on the real callers. To A/B a solution in one JMH run, add a temporary `@volatile var` toggle to the lib method and a `@Param` bench in the solution's package (the solution objects are `private`). Revert both afterwards.
  - The exception is overrides that bring a type in line with `Iterable` (`knownSize`, `sum`). They land for consistency, with their numbers reported either way.
- Settled (#192): the Scala 3 `-opt` optimizer stays off, and inline/specialized traits were dropped.
