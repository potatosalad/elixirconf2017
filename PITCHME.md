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