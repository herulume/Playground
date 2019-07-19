defmodule Game do
  def guess(actual, a..b) when is_integer(a) and is_integer(b) and is_integer(actual) do
    my_guess = div(a + b, 2)
    IO.puts("Is it #{my_guess}")

    cond do
      my_guess == actual -> IO.puts("#{actual}")
      my_guess < actual -> guess(actual, (my_guess + 1)..b)
      my_guess > actual -> guess(actual, a..(my_guess - 1))
    end
  end
end
