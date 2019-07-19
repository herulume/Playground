defmodule Monitor do
  import :timer, only: [sleep: 1]

  def sad_fun do
    sleep(500)
    exit(:boom)
  end

  def run do
    res = spawn_monitor(Monitor, :sad_fun, [])
    IO.puts(inspect(res))

    receive do
      msg -> IO.puts("MESSAGE RECEIVED: #{inspect(msg)}")
    after
      1000 ->
        IO.puts("Nothing happened as far as I am concerned")
    end
  end
end

Monitor.run()
