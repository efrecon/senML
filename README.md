# Sen(S)ML JSON Parser

This project provides a Tcl parser for [SenML] JSON. More precisely, this
implements at first a [SenSML] parser. The [SenSML] parser is wrapped for easier
access to SenML data. SenSML comprises of unfinished JSON arrays that will grow
with time, as part of a never-ending HTTP stream, for example.

The SenSML parser provide callbacks with the content of each [resolved][resolve]
[pack] represented as a Tcl dictionary.

  [SenML]: https://tools.ietf.org/html/rfc8428
  [SenSML]: https://tools.ietf.org/html/rfc8428#section-4.8
  [resolve]: https://tools.ietf.org/html/rfc8428#section-5.1.4
  [pack]: https://tools.ietf.org/html/rfc8428#section-3

## Examples

A number of examples are provided in the [demos] directory.

  [demos]: ./demos/

## Usage

### SenSML

Use the `sensml` command to create a new SenSML parsing context, the command
takes a number of dash-led options to control its behaviour. The command will
return a token which is also a command, to be used for all further operation
within this context, Tk-style. Dash-led options are:

* `-version`: the version of the data that a context will accepts. This will be
  checked against the base version, [`bver`][bver]. This defaults to `10` to
  match the [RFC][SenML].
* `-log`: the file descriptor where to send log messages. This defaults to
  `stderr` and can be set to an empty string to entirely turn off logging.
* `-level`: the log level, should be one of the following strings (upper or
  lower case), in increasing order of verbosity: `ERROR`, `WARN`, `NOTICE`,
  `INFO`, `DEBUG`, `TRACE`.
* `-callback` a command to be called back as parsing progresses. This command
  will one or two arguments. First will be a step, i.e. exactly one of `OPEN`,
  `PACK` or `CLOSE`, sent, respectively, at the beginning of a SenSML JSON
  array, for each resolved pack of the array, and at the end of the array. When
  the step is `PACK`, the callback will also take another argument which will be
  a Tcl dictionary with the content of the [resolved][resolve] [pack].

By construction, [SenSML] needs to deal with incomplete JSON. This is because,
in regular mode of operation, the source would have produced the beginning of a
JSON array, possibly a number of JSON objects, and will continue to generate
JSON objects in the array as data is acquired. This means that the SenSML
implementation need to cope with incomplete JSON arrays if it is to dynamically
generate callbacks as data flows. Given a context, there are two ways to use the
parser to cope with the situation. Usage of these cannot be mixed, you will have
to choose one of the modes of operation.

  [bver]: https://tools.ietf.org/html/rfc8428#section-4.4

### Streaming Mode

In the first mode of operation, you iteratively feed the context with blocks of
partial JSON, as data is coming from the source. The parser will use textual
heuristics at the string level to cut incoming data into some blocks that can be
understood as JSON. In practice, there is a possibility for these heuristics to
fail. Provided the variable `s` is a context returned by `sensml`, and `data` is
a possibly incomplete block containing partial JSON, you can iteratively push
data to the parser using the `stream` sub-command with the `data` block:

```tcl
$s stream $data
```

This will generate callbacks for every complete JSON object, as a
[resolved][resolve] [pack]. Any remaining JSON after the last complete object
will be kept and appended to the beginning of the next block passed through the
`stream` sub-command.

### Manual Mode

In manual mode, you should be responsible for the JSON pre-parsing and only send
JSON objects ready for resolution to the SenSML parser. You would signal about
the beginning of an array with the sub-command `begin`, iteratively push JSON
objects in the parser using multiple calls to `jsonpack`, and signal about the
end of the stream and array with `end`. So provided a variable `s` for an
existing context, sending this SenML block:

```json
[
  { "n": "test", "v": 45 },
  { "n": "test", "v": 56.7 }
]
```

could be done with the following calls:

```tcl
$s begin
$s jsonpack "{ \"n\": \"test\", \"v\": 45 }"
$s jsonpack "{ \"n\": \"test\", \"v\": 56.7 }"
$s end
```

In manual mode, there cannot be errors coming from string operations heuristics
as the command `jsonpack` is required to be fed with complete JSON objects in
the format specified by [SenML]. If you wanted to parse JSON with a different
parser that the one from [tcllib], you can feed this SenSML implementation with
the `dictpack` sub-command instead. The sub-command expects a Tcl dictionary
representing the JSON object to parse.

  [tcllib]: https://core.tcl-lang.org/tcllib/doc/trunk/embedded/md/tcllib/files/modules/json/json.md

### SenML

The [SenML] library implements an ensemble. The core of the library is a stream
parser. To parse a JSON array in SenML format and generate callbacks with Tcl
dictionaries representing the [resolved][resolve] [packs][pack] from the
original array, call the command `senml streamer`. The command takes the same
dash-led options as the main [`sensml`](#sensml) command.

To resolve a JSON array, i.e. generate a JSON array where the value of all base
fields have been integrated into the value of the fields and all time values are
absolute, call the command `senml resolve`. The command takes the same dash-led
options as the main [`sensml`](#sensml) command, at the exception of the
`-callback` option which is used internally for the reconstruction of the JSON
array.

## Acknowledgments

The development of this library has been sponsored by
[Lindborg Systems](http://lsys.se/).
