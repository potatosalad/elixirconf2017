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

---

## Yielding NIF (timeslice)

> This approach is always preferred over the other alternatives.
> 
> <small>&mdash; [ERTS 9.0 erl_nif docs](http://erlang.org/doc/man/erl_nif.html#lengthy_work)</small>

![Image](assets/down-arrow.png)

+++

### Bogdan/Björn's<br>Erlang<br>Abstract<br>Machine

<h1 class="fragment">BEAM</h1>

+++

## BEAM Scheduling

> The scheduler is responsible for the [soft] real-time guarantees of the system.
> 
> <small>&mdash; [The BEAM Book](https://happi.github.io/theBeamBook/)</small>

+++?image=https://cdn.rawgit.com/potatosalad/elixirconf2017/master/assets/preemptive-scheduling.svg&size=contain

+++?image=https://cdn.rawgit.com/potatosalad/elixirconf2017/master/assets/cooperative-scheduling.svg&size=contain

+++

## BEAM Scheduling

> One can describe the scheduling in BEAM as preemptive scheduling on top of cooperative scheduling.
> 
> <small>&mdash; [The BEAM Book](https://happi.github.io/theBeamBook/)</small>

+++

## Preemptive vs Cooperative

<ul>
<li class="fragment">Elixir functions are preemptive</li>
<li class="fragment">C functions are cooperative<span class="fragment">&hellip;hopefully</span></li>
</ul>

+++

# Elixir Functions

+++

> A process can only be suspended at certain points of the execution, such as at a receive or a function call.
> 
> <small>&mdash; [The BEAM Book](https://happi.github.io/theBeamBook/)</small>

+++

## Estimating Time

> When a process is scheduled it will get a number of reductions defined by `CONTEXT_REDS` (currently 4000).
> 
> <small>&mdash; [The BEAM Book](https://happi.github.io/theBeamBook/)</small>

+++

## What is a Reduction?

> It is not completely defined what a reduction is, but at least each function call should be counted as a reduction.
> 
> <small>&mdash; [The BEAM Book](https://happi.github.io/theBeamBook/)</small>

+++

```elixir
def echo(term) do
  term
end
```

+++

```elixir
self = :erlang.self()
{:reductions, r1} = :erlang.process_info(self, :reductions)
:ok = echo(:ok)
{:reductions, r2} = :erlang.process_info(self, :reductions)
rdiff = r2 - r1 # ~400
```

+++

```elixir
def collect(_ref, 0, replies) do
  replies
end
def collect(ref, n, replies) do
  receive do
    {^ref, reply} ->
      collect(ref, n - 1, [reply | replies])
  end
end
```

<span class="fragment current-only" data-code-focus="1,4">suspend caller of `collect/3`</span>
<span class="fragment current-only" data-code-focus="5">suspend on receive</span>
<span class="fragment current-only" data-code-focus="7">suspend on call to `collect/3`</span>
<span class="fragment current-only" data-code-focus="1-9">at least 3 preemption points</span>

+++

```elixir
parent = self()
ref = make_ref()
:ok =
  Enum.reduce(1..1000, :ok, fn (_, ok) ->
    _ = spawn(:erlang, :send, [parent, {ref, 1}])
    ok
  end)
{:reductions, r1} = :erlang.process_info(parent, :reductions)
replies = collect(ref, 1000, [])
{:reductions, r2} = :erlang.process_info(parent, :reductions)
1000 = Enum.sum(replies)
rdiff = r2 - r1 # ~1400
```

+++

# C Functions

+++

> There is a risk that a function implemented in C takes many more clock cycles per reduction than a normal Erlang function.
> 
> <small>&mdash; [The BEAM Book](https://happi.github.io/theBeamBook/)</small>

+++

```c
nif_bif_result = (*fp)(&env, bif_nif_arity, reg);
```

<small>[`erts/emulator/beam/bif_instrs.tab`](https://github.com/erlang/otp/blob/81a6adab693a75f89bc87911ac23a21308673d2d/erts/emulator/beam/bif_instrs.tab#L434-L438)</small>

<span class="fragment">blocks thread until the NIF returns</span>

+++

```c
static ERL_NIF_TERM
echo(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  return argv[0];
}
```

+++

```elixir
self = :erlang.self()
{:reductions, r1} = :erlang.process_info(self, :reductions)
:ok = :my_nif.echo(:ok)
{:reductions, r2} = :erlang.process_info(self, :reductions)
rdiff = r2 - r1 # ~200
```

+++

```c
static ERL_NIF_TERM
echo(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  (void) spinsleep(1000 * 1000); // spin for 1 second
  return argv[0];
}
```

<span class="fragment">reductions = <code>~200</code></span>

+++

### `enif_consume_timeslice`

+++

```c
static ERL_NIF_TERM
echo(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  (void) enif_consume_timeslice(env, 100);
  return argv[0];
}
```

<span class="fragment">reductions = <code>~4200</code></span>

+++

```c
if (microseconds > 1000) {
  ERL_NIF_TERM newargv[1];
  newargv[0] = enif_make_int64(env, (ErlNifSInt64) start);
  newargv[1] = enif_make_int64(env, (ErlNifSInt64) stop);
  newargv[2] = enif_make_int64(env, 1000 * 1000);
  return enif_schedule_nif(env, "spinsleep_timeslice", 0,
                           spinsleep_ts, 3, newargv);
}
```

<span class="fragment current-only" data-code-focus="5">`max_per_slice`</span>
<span class="fragment current-only" data-code-focus="1-8"></span>

+++

```c
static ERL_NIF_TERM
spinsleep_ts(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifTime start, stop, current;
  ErlNifSInt64 max_per_slice, offset = 0;
  int percent, total = 0;
  if (argc != 3
      || !enif_get_int64(env, argv[0], &start)
      || !enif_get_int64(env, argv[1], &stop)
      || !enif_get_int64(env, argv[2], &max_per_slice)) {
    return enif_make_badarg(env);
  }
  // ...
}
```

+++

```c
current = enif_monotonic_time(ERL_NIF_NSEC);
while (stop > current) {
  (void)spin(max_per_slice);
  offset += max_per_slice;
  diff = enif_monotonic_time(ERL_NIF_NSEC) - current;
  current += diff;
  percent = (int)(diff / 1000 / 1000);
  total += percent;
  if (enif_consume_timeslice(env, percent)) {
    // ...
  }
}
return enif_make_int64(env, (current - start) / 1000);
```

+++

```c
max_per_slice = offset;
if (total > 100) {
  int m = (int)(total / 100);
  if (m == 1) {
    max_per_slice -= (max_per_slice * (total - 100) / 100);
  } else {
    max_per_slice = (max_per_slice / m);
  }
}
ERL_NIF_TERM newargv[1];
newargv[0] = argv[0]; // start
newargv[1] = argv[1]; // stop
newargv[2] = enif_make_int64(env, max_per_slice);
return enif_schedule_nif(env, "spinsleep_timeslice", 0,
                         spinsleep_ts, 3, newargv);
```

+++

![spinsleep_timeslice-1ms](https://cdn.rawgit.com/potatosalad/elixirconf2017/master/assets/idle/spinsleep_timeslice-1ms.svg)

+++

![spinsleep_timeslice-10ms](https://cdn.rawgit.com/potatosalad/elixirconf2017/master/assets/idle/spinsleep_timeslice-10ms.svg)

+++

![spinsleep_timeslice-100ms](https://cdn.rawgit.com/potatosalad/elixirconf2017/master/assets/idle/spinsleep_timeslice-100ms.svg)

+++

![spinsleep_timeslice-1s](https://cdn.rawgit.com/potatosalad/elixirconf2017/master/assets/idle/spinsleep_timeslice-1s.svg)

+++

![spinsleep_timeslice-10s](https://cdn.rawgit.com/potatosalad/elixirconf2017/master/assets/idle/spinsleep_timeslice-10s.svg)