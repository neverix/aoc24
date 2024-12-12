defmodule SharedCache do
  def start_link do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  def update(updater) do
    Agent.update(__MODULE__, updater)
  end

  def write(key, value) do
    Agent.update(__MODULE__, fn map -> Map.put(map, key, value) end)
  end

  def read(key) do
    Agent.get(__MODULE__, fn map -> Map.get(map, key) end)
  end

  def get_all do
    Agent.get(__MODULE__, fn map -> map end)
  end
end

defmodule D11P1 do
  def find_blink(num, remaining) do
    case remaining do
      0 -> 1
      n ->
        if (num == 0) do
          find_blink(1, remaining-1)
        else
          ts = num |> to_string |> to_char_list
          if (rem(length(ts), 2) == 0) do
            hl = Integer.floor_div(length(ts), 2)
            {ts_1, ts_2} = {Enum.slice(ts, 0, hl), Enum.slice(ts, hl, length(ts) - hl)}
            {ts_1, ts_2} = {String.to_integer(ts_1 |> to_string), String.to_integer(ts_2 |> to_string)}
            find_blink(ts_1, remaining-1) + find_blink(ts_2, remaining - 1)
          else
            find_blink(num * 2024, remaining - 1)
          end
        end
    end
  end

  def main() do
    filename = "input.txt"
    numbers = case File.read(filename) do
      {:ok, content} ->
        numbers =
          content
          |> String.trim()  # Remove leading/trailing whitespace
          |> String.split(~r/\s+/)  # Split by one or more whitespace characters
          |> Enum.map(&String.to_integer/1)  # Convert strings to floats

        numbers

      {:error, reason} ->
        raise reason
    end
    IO.inspect(numbers)
    {:ok, _} = SharedCache.start_link()

    IO.inspect(numbers |> Enum.map(&(find_blink(&1, 25))) |> Enum.reduce(0, &(&1 + &2)))
    # IO.inspect(find_blink(125, 25) + find_blink(17, 25))
  end
end

D11P1.main()
