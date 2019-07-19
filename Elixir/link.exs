defmodule Link do
  import :timer, only: [sleep: 1]

  def sad_fun do
    sleep(500)
    exit(:boom)
  end

  def run do
    # Process.flag(:trap_exit, true) # Trap the exit
    spawn_link(Link, :sad_fun, [])

    receive do
      msg -> IO.puts("MESSAGE RECEIVED: #{inspect(msg)}")
    after
      1000 ->
        IO.puts("Nothing happened as far as I am concerned")
    end
  end
end

Link.run()
