# servant-client-js

This is an experimental Servant client for frontend JavaScript. It serves the same purpose as [servant-client-ghcjs](https://github.com/haskell-servant/servant/tree/master/servant-client-ghcjs) and [servant-client-jsaddle](https://github.com/haskell-servant/servant/tree/master/servant-client-jsaddle). It is at least for most purposes able to serve as a drop-in replacement for either of those Servant clients. However, it has some significant differences. As compared to the official JavaScript Servant clients, servant-client-js:

* supports streaming HTTP responses;
* is a single code module with which works in both GHCJS and JSaddle;
* uses the [Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/) as opposed to [XMLHttpRequest](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/);
* uses the JSaddle API and not the GHCJS FFI;
* is less well-tested.

## Building

I recommend building servant-client-js using Nix:

```
servant-client-js]$ nix-build
servant-client-js]$ nix-build --arg isJS true
```

or:

```
servant-client-js]$ nix-shell
[nix-shell]$ cabal repl
```

## Usage

Use servant-client-js in conjunction with Servant, as you would any other Servant client.

Here is [a usage example](https://github.com/morganthomas/streaming-table-test).
