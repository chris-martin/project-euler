defmodule Problem1 do
  def answer(), do: 1..999 |> Stream.filter(&(ok? &1)) |> Enum.sum
  def ok?(x), do: (rem(x, 3) == 0) or (rem(x, 5) == 0)
end
