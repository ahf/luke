

# Luke #

__Version:__ 1.3.0

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

Luke is an Erlang NIF for the post-quantum key exchange: A New Hope.

For more information about A New Hope, including the paper itself, see [github.com/tpoeppelmann/newhope](https://github.com/tpoeppelmann/newhope)
and [cryptojedi.org](https://cryptojedi.org/crypto/#newhope).


### <a name="Example_Usage">Example Usage</a> ###

1. Alice generates a new keypair and sends her public key to Bob.

```erlang
#{ secret := AliceSecretKey, public := AlicePublicKey } = luke:keypair().
```

2. Bob uses Alice's public key to compute the shared secret and a public key that he then sends back to Alice.

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
<tr><td><a href="https://lab.baconsvin.org/ahf/luke/blob/develop/doc/luke.md" class="module">luke</a></td></tr></table>

