

# Module luke_tor #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

New Hope Tor implementation.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="description"></a>

## Description ##

This module contains the exact same API as the luke module, but uses the
Tor implementation of New Hope instead of the reference implementation.

The Tor implementation of New Hope uses a different, constant-time,
generation of the polynomial generated in the keypair/0 and sharedb/1
functions.

This module should only be used if you are planning on interfacing against
the Tor network.

<a name="types"></a>

## Data Types ##




### <a name="type-keypair">keypair()</a> ###


<pre><code>
keypair() = #{secret =&gt; <a href="#type-secret_key">secret_key()</a>, public =&gt; <a href="#type-public_key">public_key()</a>}
</code></pre>




### <a name="type-public_key">public_key()</a> ###


<pre><code>
public_key() = binary()
</code></pre>




### <a name="type-secret_key">secret_key()</a> ###


<pre><code>
secret_key() = binary()
</code></pre>




### <a name="type-shared">shared()</a> ###


<pre><code>
shared() = binary()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#keypair-0">keypair/0</a></td><td>Generate a New Hope keypair.</td></tr><tr><td valign="top"><a href="#shareda-2">shareda/2</a></td><td>Compute the shared secret from a secret key and a public key.</td></tr><tr><td valign="top"><a href="#sharedb-1">sharedb/1</a></td><td>Generate shared secret and ephemeral public key from a public key.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="keypair-0"></a>

### keypair/0 ###

<pre><code>
keypair() -&gt; <a href="#type-keypair">keypair()</a>
</code></pre>
<br />

Generate a New Hope keypair.

This function generates and returns a new keypair for the New Hope key
exchange scheme. The return value is a map to ensure that the public key
isn't misused as the secret key and vice versa.

The public key generated here is an 1824 byte binary.

<a name="shareda-2"></a>

### shareda/2 ###

<pre><code>
shareda(SecretA::<a href="#type-secret_key">secret_key()</a>, PublicB::<a href="#type-public_key">public_key()</a>) -&gt; <a href="#type-shared">shared()</a>
</code></pre>
<br />

Compute the shared secret from a secret key and a public key.

This function takes the secret key of the initiator and the ephemeral public
key of the responder and computes the shared secret.

The shared secret is a 32 byte binary that can be used as key for a
symmetric cipher.

<a name="sharedb-1"></a>

### sharedb/1 ###

<pre><code>
sharedb(PublicA::<a href="#type-public_key">public_key()</a>) -&gt; #{shared =&gt; <a href="#type-shared">shared()</a>, public =&gt; <a href="#type-public_key">public_key()</a>}
</code></pre>
<br />

Generate shared secret and ephemeral public key from a public key.

This function takes the public key of the handshake initiator and computes
the shared secret and an ephemeral public key.

The ephemeral public key should be send back to the handshake initiator to
complete the handshake. The ephemeral public key generated here is a 2048
byte binary.

The shared secret is a 32 byte binary that can be used as key for a
symmetric cipher.

The return value of this function is a map to ensure that the return values
are not used incorrectly.

