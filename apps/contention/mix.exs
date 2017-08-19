defmodule Contention.Mixfile do
  use Mix.Project

  def project do
    [
      app: :contention,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.4",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      compilers: [:elixir_make] ++ Mix.compilers(),
      make_env: %{"MIX_ENV" => to_string(Mix.env())},
      make_clean: ["clean"],
      make_cwd: "c_src"
    ]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application() do
    # Specify extra applications you'll use from Erlang/Elixir
    [
      mod: {Contention.Application, []},
      extra_applications: [:logger]
    ]
  end

  # Dependencies can be Hex packages:
  #
  #   {:my_dep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:my_dep, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
  #
  # To depend on another app inside the umbrella:
  #
  #   {:my_app, in_umbrella: true}
  #
  # Type "mix help deps" for more examples and options
  defp deps() do
    [
      {:cowboy, github: "ninenines/cowboy", ref: "2.0.0-rc.1", override: true},
      {:cowlib, github: "ninenines/cowlib", ref: "2.0.0-rc.1", override: true},
      {:elixir_make, "~> 0.4", runtime: false},
      {:h2o, github: "potatosalad/erlang-h2o", ref: "178ae3f3515bb5f886847ca91b46194019d883e7"},
      {:ranch, "~> 1.4.0", override: true}
    ]
  end
end
