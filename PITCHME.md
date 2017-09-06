---?image=assets/TitleCard.png&size=contain
<!-- .slide: data-background-color="#4b285b" -->

---

## Well-Behaved
### Native Implemented Functions
### for Elixir

<hr>

<small>[gitpitch.com/potatosalad/elixirconf2017](https://gitpitch.com/potatosalad/elixirconf2017)</small>

---

## Andrew Bennett
#### <a href="https://github.com/potatosalad" style="color: black;"><img src="https://cdn.rawgit.com/potatosalad/elixirconf2017/master/assets/github-mark.svg" width="40" height="40" border="0" style="border: none; box-shadow: none; margin: 0; padding: 0;"> potatosalad</a>
#### <a href="https://twitter.com/potatosaladx" style="color: black;"><img src="https://cdn.rawgit.com/potatosalad/elixirconf2017/master/assets/twitter-mark.gif" width="40" height="40" border="0" style="border: none; box-shadow: none; margin: 0; padding: 0;"> potatosaladx</a>

![Image](assets/down-arrow.png)

+++

<img src="https://cdn.rawgit.com/potatosalad/elixirconf2017/master/assets/Vista.svg" alt="Vista" border="0" style="border: none; box-shadow: none;">

---

> Native Implemented Functions (NIFs)<br>still experimental but very useful.
> 
> Feedback is welcome.
> 
> <small>&mdash; [OTP-R13B03 announcement (25 Nov 2009)](http://www.erlang.org/news/32)</small>

![Image](assets/down-arrow.png)

+++

<table>
  <tr>
    <th>Type</th>
    <th>Isolation</th>
    <th>Latency*</th>
  </tr>
  <tr>
    <td><code>Node</code></td>
    <td style="background-color: #cfc;"><code>Network</code></td>
    <td style="background-color: #fcc; text-align: right;"><code>~100μs</code></td>
  </tr>
  <tr>
    <td><code>Port</code></td>
    <td style="background-color: #ffc;"><code>Process</code></td>
    <td style="background-color: #fcc; text-align: right;"><code>~100μs</code></td>
  </tr>
  <tr>
    <td><code>Port Driver</code></td>
    <td style="background-color: #fcc;"><code>Shared</code></td>
    <td style="background-color: #ffc; text-align: right;"><code>~10μs</code></td>
  </tr>
  <tr>
    <td><code>NIF</code></td>
    <td style="background-color: #fcc;"><code>Shared</code></td>
    <td style="background-color: #cfc; text-align: right;"><code>~0.1μs</code></td>
  </tr>
</table>
<br>
<small>&#42;Rounded to nearest order of magnitude.<br><a href="https://potatosalad.io/2017/08/05/latency-of-native-functions-for-erlang-and-elixir" target="_blank" style="font-size: 0.75em;">potatosalad.io/2017/08/05/latency-of-native-functions-for-erlang-and-elixir</a></small>

+++

# Don't write a NIF unless you have to

---

> A well-behaving native function is to return to its caller within 1 millisecond.
> 
> <small>&mdash; [ERTS 9.0 erl_nif docs](http://erlang.org/doc/man/erl_nif.html#lengthy_work)</small>

![Image](assets/down-arrow.png)

+++

```c
void
spin(ErlNifSInt64 count)
{
  for (; count > 0; --count) {}
}
```

+++

```c
ErlNifSInt64
spinsleep(ErlNifSInt64 microseconds)
{
  ErlNifTime start, current, stop;
  ErlNifSInt64 count;
  start = enif_monotonic_time(ERL_NIF_NSEC);
  stop = start + ((ErlNifTime)microseconds * 1000);
  do {
    current = enif_monotonic_time(ERL_NIF_NSEC);
    count = (stop - current) / 2;
    (void)spin(count);
  } while (stop > current);
  return ((current - start) / 1000);
}
```

<span class="fragment current-only" data-code-focus="2">keeps CPU busy for given μs</span>
<span class="fragment current-only" data-code-focus="6"></span>
<span class="fragment current-only" data-code-focus="6-7">stop time = start time + given μs</span>
<span class="fragment current-only" data-code-focus="8-12">loop until stop time reached</span>
<span class="fragment current-only" data-code-focus="1-14"></span>

+++

```elixir
def spinsleep(microseconds, multiplier) do
  function = fn () ->
    :my_nif.spinsleep(microseconds)
  end
  count = :erlang.system_info(:schedulers_online) * multipler
  spawn_multiple(function, count)
end
```

<span class="fragment current-only" data-code-focus="2-4">busy sleep for given μs</span>
<span class="fragment current-only" data-code-focus="5">spawns this many processes</span>
<span class="fragment current-only" data-code-focus="1-7"></span>

+++

![spinsleep-1ms](https://cdn.rawgit.com/potatosalad/elixirconf2017/master/assets/idle/spinsleep-1ms.svg)

+++

![spinsleep-10ms](https://cdn.rawgit.com/potatosalad/elixirconf2017/master/assets/idle/spinsleep-10ms.svg)

+++

![spinsleep-100ms](https://cdn.rawgit.com/potatosalad/elixirconf2017/master/assets/idle/spinsleep-100ms.svg)

+++

![spinsleep_timeslice-100ms](https://cdn.rawgit.com/potatosalad/elixirconf2017/master/assets/idle/spinsleep_timeslice-100ms.svg)

---

### Long-running NIF Causes

<br>

 * Variable input/output
   * `binary`
   * `list`
   * `map`
   * `tuple`
 * CPU blocking
 * I/O blocking

---

### Long-running NIF Solutions

<br>

<ol>
  <li>Dirty NIF</li>
  <li>Yielding NIF (timeslice)</li>
  <li>Yielding Dirty NIF</li>
  <li class="fragment">Threaded NIF</li>
</ol>

---

## Dirty NIF

> A NIF that cannot be split and cannot execute in a millisecond or less.
> 
> <small>&mdash; [ERTS 9.0 erl_nif docs](http://erlang.org/doc/man/erl_nif.html#lengthy_work)</small>

![Image](assets/down-arrow.png)

+++

```c
{"spinsleep", 1, spinsleep},
{"spinsleep_dirty", 1, spinsleep, ERL_NIF_DIRTY_JOB_CPU_BOUND},
```

<span class="fragment current-only" data-code-focus="1">normal NIF</span>
<span class="fragment current-only" data-code-focus="2">dirty NIF</span>
<span class="fragment current-only" data-code-focus="1-2"></span>

+++

![spinsleep_dirty-1ms](https://cdn.rawgit.com/potatosalad/elixirconf2017/master/assets/idle/spinsleep_dirty-1ms.svg)

+++

![spinsleep_dirty-10ms](https://cdn.rawgit.com/potatosalad/elixirconf2017/master/assets/idle/spinsleep_dirty-10ms.svg)

+++

![spinsleep_dirty-100ms](https://cdn.rawgit.com/potatosalad/elixirconf2017/master/assets/idle/spinsleep_dirty-100ms.svg)

+++

### &ldquo;Dirty NIF&rdquo; does not mean &ldquo;Faster NIF&rdquo;