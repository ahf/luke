

# Luke #

__Version:__ 1.3.0

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

Luke is an Erlang NIF for the post-quantum key exchange: A New Hope.

For more information about A New Hope, including the paper itself, see [github.com/tpoeppelmann/newhope](https://github.com/tpoeppelmann/newhope)
and [cryptojedi.org](https://cryptojedi.org/crypto/#newhope).

Luke also ships with optional support for the Tor implementation of A New Hope.
Use the `luke_tor` module instead of `luke` if you have a need for that.

== Example Usage ==

1. Alice generates a new keypair and sends her public key to Bob.<pre lang="erlang">#{ secret := AliceSecretKey, public := AlicePublicKey } = luke:keypair().</pre>

2. Bob uses Alice`s public key to compute the shared secret and a public key that he then sends back to Alice.

```erlang
#{ shared := BobSharedSecret, public := BobPublicKey } = luke:sharedb(AlicePublicKey).
```

3. Alice computes the shared secret using her own secret key and Bob's public
key.

```erlang
AliceSharedSecret = luke:shareda(AliceSecretKey, BobPublicKey).
```

4. You can now verify that the shared secret is the same.

```erlang
AliceSharedSecret =:= BobSharedSecret.
```



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="luke.md" class="module">luke</a></td></tr>
<tr><td><a href="luke_tor.md" class="module">luke_tor</a></td></tr></table>

