# Isocode

Search Perl 5 sources by given code example.

Rules of matching:

* Ignore the differrence between ways of writing literal values: e.g. ` "abc" ` == `q(abc)`. Even `"123"` == `123`
* Variable names can be differrent, but must be consistent: `$a + $a` == `$b + b`, but `$a + $b` != `$c + $c`
* Whitespace, newlines and comments are ignored.

## Usage

You can give it file or string as a pattern. Also, see `--help`.

```
# isocode -v -p '$abcd + $def' ~/tmp/otrs/ 

Parsing...
[ Var 36 ( Id "abcd" )
, Op "+"
, Var 36 ( Id "def" )
]
Scanning...
/home/spek/tmp/otrs/scripts/test/SMIME.t:1
$Counter + $OriginalPrivateListCount
/home/spek/tmp/otrs/scripts/test/Ticket.t:1
$CurrentSystemTime + $Diff

...

/home/spek/tmp/otrs/Kernel/Output/HTML/FilterText/AutoLink.pm:1
$Counter + $_
/home/spek/tmp/otrs/Kernel/Output/HTML/Dashboard/AppointmentCalendar.pm:1
$CurrentSystemTime + $DateOffset
Files in directory: 5381
Scanned 2728 files with 0 errors
Total 90 files matches
Total 315 matches
```
## Why

For fun. Maybe it will help someone.

"isocode" means "isomorphic code".

## Build and run

You need `ghc` haskell compiller (I used v 8.0.2). + `cabal`.
You can get those from, e.g. `haskell-platform` packet in Ubuntu. 

```
# cabal sandbox init
# cabal install
# ./dist/build/isocode/isocode --help
isocode - search source tree by Perl code sample

Usage: isocode (PATTERN_FILE | (-p|--pattern PATTERN)) SEARCH_DIR [-v|--verbose]
               [-d|--debug-pattern] [-j|--concurrency ARG]
  Searches for code similar to PATTERN_FILE or PATTERN in SEARCH_DIR

Available options:
  -v,--verbose             spew debug information
  -d,--debug-pattern       just parse the pattern and show, do not search for it
  -j,--concurrency ARG     How many threads in parallell
  -h,--help                Show this help text
```

## How it works

'Only Perl can parse Perl', but we only need to understand perl code at very basic level (is it var? is it string literal?).

1. Parse example code to simple AST
2. Given that AST, generate parser which search and parses _only_ similar code
3. Search directory recursively, applying that parser + do variable names consistency check
4. Profit ???

Search is done by naive algorithm with simple optimization:

1. get list of first characters that code can start with (e.g. `['u']` for `use` or `['"', 'q', '\'' ... ]` for `"string"` 
2. Find next occurrence of one of this characters
3. Run parser from this position
4. If fail - go to 2

Its faster than parsing whole source, but stil may be slow for common characters.

Files are loaded and parsed concurrently, but it doesn help much :)

TLDR: Basically, its three parser generators (one for command line arguments) stiched together.

## What doesn't work

- [x] tests (more needed).
- [ ] error messages are really bad
- [ ] line numbers in matching 
- [ ] variable interpolation in strings, like `"show me $var"` treated as regular strings
- [ ] `=cut` documentation parsing
- [ ] regular expressions - only `m/.../` form is (kind of) supported, regexp parsing it much more context-sesitive
- [ ] `qw` works same way as `qq`
- [ ] no differrence between double and single quotes
- [ ] `;` and `,` must be present in same places.
- [ ] reference (`\`) is not treated in special way, it's just like any other operator
- [ ] all variables treated as they are in same namespace (e.g. blocks are not taken into account)
- [ ] some minor FIXMEs in code
- [ ] concurrency has some problems (e.g. threads are starving)

## TODO

- [ ] special markers to match any sequence, similar to `.*` or `.+` in regexps, e.g. `if ($a == $b){ .* } else {print "error"}`
- [ ] support for code generation based on matches


