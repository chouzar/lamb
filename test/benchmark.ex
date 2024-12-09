# https://www.erlang.org/doc/system/profiling.html#never-guess-about-performance-bottlenecks
# https://www.erlang.org/doc/system/profiling.html#memory-profiling
# https://www.erlang.org/doc/apps/erts/erlang#process_info/2

defmodule Lamb.Artifacts.IndexedBenchmark do
  @benchmark :benchmark
  @lamb :lamb

  def run(table) do
    Benchee.run(
      %{
        "lamb.lookup" => fn index ->
          @lamb.lookup(table, index)
        end,
        "lamb.any" => fn index ->
          @lamb.any(table, index)
        end,
        "lamb.count" => fn index ->
          @lamb.count(table, @benchmark.query_find_record(index))
        end,
        "lamb.search" => fn index ->
          @lamb.search(table, @benchmark.query_find_record(index))
        end
      },
      time: 5,
      before_each: fn _ -> @benchmark.random(100_000) end,
      print: %{configuration: false}
    )
  end
end

defmodule Lamb.Artifacts.BaggedBenchmark do
  @benchmark :benchmark
  @lamb :lamb

  def run(table) do
    Benchee.run(
      %{
        "lamb.lookup" => fn group ->
          @lamb.lookup(table, group)
        end,
        "lamb.any" => fn group ->
          @lamb.any(table, group)
        end,
        "lamb.count" => fn group ->
          @lamb.count(table, @benchmark.query_find_record(group))
        end,
        "lamb.search" => fn group ->
          @lamb.search(table, @benchmark.query_find_record(group))
        end,
        "lamb.lookup GroupD" => fn _group ->
          @lamb.lookup(table, :group_d)
        end,
        "lamb.any GroupD" => fn group ->
          @lamb.any(table, group)
        end,
        "lamb.count GroupD" => fn _group ->
          @lamb.count(table, @benchmark.query_find_record(:group_d))
        end,
        "lamb.search GroupD" => fn _group ->
          @lamb.search(table, @benchmark.query_find_record(:group_d))
        end
      },
      time: 5,
      before_each: fn _ -> Enum.random([:group_a, :group_b, :group_c]) end,
      print: %{configuration: false}
    )
  end
end
