# HTTP/2 NIF Contention Tests

To get the HTTP/2 requests per second metric, you will need to compile a modified version of nghttp2 where h2load supports the `--duration` and `--interval` flags (see [potatosalad/nghttp2@elixirconf2017](https://github.com/potatosalad/nghttp2/tree/elixirconf2017)).

Start the contention node:

```bash
iex --name 'contention@127.0.0.1' --cookie 'mycookie' -S mix
```

In another terminal, start the tests specifying the location of h2load and the name of the test.

```bash
./h2load.sh src/h2load h2-spinsleep-100ms
```

The resulting SVG files were then processed using [SVGOMG](https://jakearchibald.github.io/svgomg/) and `strip-extra.rb` was run on them to try to reduce their size.