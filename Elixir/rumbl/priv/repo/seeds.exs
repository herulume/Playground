# Script for populating the database. You can run it as:
#
#     mix run priv/repo/seeds.exs
#
# Inside the script, you can read and write to any of your
# repositories directly:
#
#     Rumbl.Repo.insert!(%Rumbl.SomeSchema{})
#
# We recommend using the bang functions (`insert!`, `update!`
# and so on) as they will fail if something goes wrong.
alias Rumbl.Accounts.User
alias Rumbl.Multimedia
alias Rumbl.Repo

Repo.insert!(
  %User{}
  |> User.registration_changeset(%{
        name: "herulume",
        username: "herulume",
        password: "qwerty123"
}), on_conflict: :nothing)


for category <- ~w(Action Drama Romance Comedy Sci-fi) do
    Multimedia.create_category!(category)
end
