defmodule ForcedMonitor do
  import :timer, only: [sleep: 1]

  def meh(addr) do
    send(addr, "gaj")
    raise "Giving up"
  end

  def run do
    spawn_monitor(ForcedMonitor, :meh, [self()])
    sleep(500)

    receive do
      msg -> IO.puts("MESSAGE RECEIVED: #{inspect(msg)}")
    after
      1000 ->
        IO.puts("Nothing happened as far as I am concerned")
    end
  end
end

ForcedMonitor.run()
