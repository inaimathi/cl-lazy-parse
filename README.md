# cl-lazy-parse
###### A non-blocking, stream-ba... ah fuck it, I'll do it later...

This is a non-blocking, stream-based parser library. The original use case was parsing incoming HTTP messages for a non-blocking server, but it might be a benefit anywhere that

1. Needs to read data from untrusted sources *(where you therefore can't assume the availability of the entire message)*
2. In non-trivial machine formats
3. Where some part of the incoming message will determine how the rest of it should be treated

### Trivial Example

CL-USER> (ql:quickload :cl-lazy-parse)
To load "cl-lazy-parse":
  Load 1 ASDF system:
    cl-lazy-parse
; Loading "cl-lazy-parse"
[package cl-lazy-parse]...
(:CL-LAZY-PARSE)

CL-USER> (in-package :cl-lazy-parse)
#<PACKAGE "CL-LAZY-PARSE">

CL-LAZY-PARSE> (run! "testing" "test")
(#\t #\e #\s #\t)
4

CL-LAZY-PARSE> (run! "testing" "nope")
#:FAIL1129

CL-LAZY-PARSE> (run! "testing" (and>> (char>> #\t) (many>> (not-char>> #\t))))
(#\t (#\e #\s))
3

CL-LAZY-PARSE> 

### Exported Functions

`and>>`
`or>>`
`many>>`
`char>>`
`not-char>>`

`with`
`_fn`
`failed?`

### TODO

- Define `run-many!`, which should run the given parser as many times as possible, resetting the input buffer each time
- Make the invocation of `run!` on a `string` more efficient (it should just start with said string as the initial buffer contents; no stream stuff involved).
