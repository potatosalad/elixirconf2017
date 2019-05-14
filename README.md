# Well-Behaved Native Implemented Functions for Elixir
### ElixirConf 2017 &mdash; 5-8 September, 2017 &mdash; Bellevue, Washington

[![GitPitch](https://gitpitch.com/assets/badge.svg)](https://gitpitch.com/potatosalad/elixirconf2017/master?grs=github&t=white)

<a href="http://www.youtube.com/watch?feature=player_embedded&v=FYQcn9zcZVA
" target="_blank"><img src="http://img.youtube.com/vi/FYQcn9zcZVA/0.jpg" 
alt="YouTube: ElixirConf 2017 - Well Behaved Native Implemented Functions for Elixir - Andrew Bennett" width="240" height="180" border="10" /><br><small>YouTube: ElixirConf 2017 - Well Behaved Native Implemented Functions for Elixir - Andrew Bennett</small></a>

This repository contains the sources used for the [&ldquo;Well-Behaved Native Implemented Functions for Elixir&rdquo;](https://web.archive.org/web/20170903202036/https://elixirconf.com/speakers#andrew_bennett) presentation given at ElixirConf 2017 on Friday, September 8, 2017 in Bellevue, Washington.

There is a post-talk discussion thread available on Elixir Forum: [elixirforum.com/t/post-talk-discussion-well-behaved-native-implemented-functions-for-elixir/8472](https://elixirforum.com/t/post-talk-discussion-well-behaved-native-implemented-functions-for-elixir/8472)

Feel free to [file an issue](https://github.com/potatosalad/elixirconf2017/issues) to this repository if you have any questions related to the talk.

The presentation slides are available at [gitpitch.com/potatosalad/elixirconf2017](https://gitpitch.com/potatosalad/elixirconf2017) and as [a PDF](https://cdn.rawgit.com/potatosalad/elixirconf2017/master/presentation.pdf).

### Resources

 * [Latency of Native Functions for Erlang and Elixir](https://potatosalad.io/2017/08/05/latency-of-native-functions-for-erlang-and-elixir)
 * [Load Testing cowboy 2.0.0-rc.1](https://potatosalad.io/2017/08/20/load-testing-cowboy-2-0-0-rc-1)

##### NIF Related

 * [`erl_nif` documentation](http://erlang.org/doc/man/erl_nif.html)
 * [Steve Vinoski - Optimizing Native Code for Erlang](https://www.youtube.com/watch?v=57AkoJfojK8)
   * [basho/nifwait](https://github.com/basho/nifwait)
   * [vinoski/bitwise](https://github.com/vinoski/bitwise)
 * [Julian Squires - Erlang Factory SF 2016 - What If Your NIF Goes Adrift](https://www.youtube.com/watch?v=5Qkqs2oNboA)
   * [tokenrove/niffy](https://github.com/tokenrove/niffy)
 * Sverker Eriksson's `enif_select` examples
   * [`sverk_tcp_nif.c`](https://github.com/potatosalad/elixirconf2017/blob/master/apps/select/c_src/sverk_tcp/sverk_tcp_nif.c)
   * [`ttsl_nif.c`](https://github.com/sverker/otp/blob/sverker/enif_select-examples/erts/emulator/nifs/unix/ttsl_nif.c)

##### BEAM Related

 * [The BEAM Book](https://happi.github.io/theBeamBook/)
 * [Hamidreza Soleimani - Erlang Scheduler Details and Why It Matters](https://hamidreza-s.github.io/erlang/scheduling/real-time/preemptive/migration/2016/02/09/erlang-scheduler-details.html)
 * [Lukas Larsson - Understanding the Erlang Scheduler](https://www.youtube.com/watch?v=tBAM_N9qPno)
 * [Lukas Larsson - Lambda Days - Scheduling in the Erlang VM](https://www.youtube.com/watch?v=_i0AscBx3vk)
 * [Robert Virding - Hitchhiker's Tour of the BEAM](https://www.youtube.com/watch?v=_Pwlvy3zz9M)
