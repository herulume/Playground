# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :rumbl,
  ecto_repos: [Rumbl.Repo]

# Configures the endpoint
config :rumbl, RumblWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "GeDgGGnHr22gw4fbc0zPcvkEkn1H1kKLcNZsdhL5+fTiLP8bKAY5fst7prE5mdAj",
  render_errors: [view: RumblWeb.ErrorView, accepts: ~w(html json), layout: false],
  pubsub_server: Rumbl.PubSub,
  live_view: [signing_salt: "AWe/Tcs6"]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"