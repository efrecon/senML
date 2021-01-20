# Sen(S)ML JSON Parser

This project provides a Tcl parser for [SenML] JSON. More precisely, this
implements at first a [SenSML] parser. The [SenSML] parser is wrapped for easier
access to SenML data. SenSML comprises of unfinished JSON arrays that will grow
with time, as part of a never-ending HTTP stream, for example.

The SenSML parser provide callbacks with the content of each [resolved] [pack]
represented as a Tcl dictionary.

  [SenML]: https://tools.ietf.org/html/rfc8428
  [SenSML]: https://tools.ietf.org/html/rfc8428#section-4.8
  [resolved]: https://tools.ietf.org/html/rfc8428#section-5.1.4
  [pack]: https://tools.ietf.org/html/rfc8428#section-3

## Examples

A number of examples are provided in the [demos] directory.

  [demos]: ./demos/
  