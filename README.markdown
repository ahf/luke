# Luke

Luke is an Erlang NIF for the post-quantum key exchange: A New Hope.

For more information about A New Hope, including the paper itself, see:
https://github.com/tpoeppelmann/newhope

## Usage

1. Alice generates a new keypair.

        #{ secret := AliceSecretKey, public := AlicePublicKey } = luke:keypair().

2. Bob uses Alice's public key to compute the shared secret and a public key that
   he then sends to Alice.

        #{ shared := BobSharedSecret, public := BobPublicKey } = luke:sharedb(AlicePublicKey).

3. Alice computes the shared secret using her own secret key and Bob's public
   key.

        AliceSharedSecret = luke:shareda(AliceSecretKey, BobPublicKey).

4. You can now verify that the shared secret is the same.

        AliceSharedSecret =:= BobSharedSecret.
