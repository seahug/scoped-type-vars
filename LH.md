# Literate Haskell template

This is a template for a simple single-source-file Literate Haskell project which might be useful for simple presentations of straightforward Haskell programming constructs or concepts.

Inspired by:

* [Evan Borden][eborden]
* [`markdown-unlit`'s documentation][markdown-unlit]

To get a live preview as it would look on GitHub:

* [`grip`][grip] is awesome
* Use wrapper script at `script/grip` if you feel so inclined
* This will open a web server at [http://localhost:6419][server] where you can preview your content

To automatically rebuild Literate Haskell project on changes:

```bash
stack build --file-watch --fast --exec scoped-typed-vars
```

## Licence

Released under MIT License

Copyright &copy; 2016 Richard Cook

[eborden]: https://github.com/eborden/json-msg-pack
[grip]: https://github.com/joeyespo/grip
[markdown-unlit]: https://github.com/sol/markdown-unlit#readme
[server]: http://localhost:6419
