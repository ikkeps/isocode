# Isocode
![Docker Image CI](https://github.com/ikkeps/isocode/workflows/Docker%20Image%20CI/badge.svg?branch=master)

Search Perl 5 sources by given code example.

You can give it file or string as a pattern. Command line interface is compatible with `grep`, so in common cases you can replace `grep` with `isocode`.
See `--help` for all supported options.

```
$ ./isocode -nr -e 'my ( $type, %args ) = @_; my $a = {}; bless( $a, $type);' ~/tmp/otrs/ -W
Parsing...
[ Id "my"
, Block "("
    [ Var 36 ( Id "type" )
    , Sep 44
    , Var 37 ( Id "args" )
    , Optional
        ( Choice
            [ Sep 44
            , Sep 59
            ]
        )
    ] ")"
, Op "="
, Var 64 ( Id "_" )
, Sep 59
, Id "my"
, Var 36 ( Id "a" )
, Op "="
, Block "{" [] "}"
, Sep 59
, Id "bless"
, Block "("
    [ Var 36 ( Id "a" )
    , Sep 44
    , Var 36 ( Id "type" )
    , Optional
        ( Choice
            [ Sep 44
            , Sep 59
            ]
        )
    ] ")"
, Sep 59
]

Scanning...
/home/spek/tmp/otrs/scripts/test/Layout/Template/OutputFilter.pm:15:5
my ( $Type, %Param ) = @_;

    # allocate new hash for object
    my $Self = {};
    bless( $Self, $Type );
/home/spek/tmp/otrs/scripts/test/Layout/Template/OutputFilterInclude.pm:15:5
my ( $Type, %Param ) = @_;

    # allocate new hash for object
    my $Self = {};
    bless( $Self, $Type );
/home/spek/tmp/otrs/scripts/test/CommunicationChannel/Test.pm:19:5
my ( $Type, %Param ) = @_;

    my $Self = {};
    bless( $Self, $Type );

...

/home/spek/tmp/otrs/Kernel/Output/PDF/Ticket.pm:32:5
my ( $Type, %Param ) = @_;

    # Allocate new hash for object.
    my $Self = {};
    bless( $Self, $Type );
Files in directory: 5381
Scanned 2728 files with 0 errors
Total 372 files matches
Total 372 matches
```

Rules of matching:

* Ignore the differrence between ways of writing literal values: e.g. ` "abc" ` == `q(abc)`. Even `"123"` == `123`
* Variable names can be differrent, but must be consistent: `$a + $a` == `$b + b`, but `$a + $b` != `$c + $c`
* Whitespace, newlines and comments are ignored.

## Why

For fun. Maybe it will help someone.

"isocode" means "isomorphic code".

## Build and run

You need `ghc` haskell compiller (I used v 8.0.2). + `cabal`.
You can get those from, e.g. `haskell-platform` packet in Ubuntu. 

```
# ./build-in-docker.sh
# ./isocode --help
isocode - search source tree by Perl code sample

Usage: isocode (PATTERN | (-e|--regexp PATTERN) | (-f|--file PATTERN_FILE))
               SEARCH_DIR [-W|--verbose] [-Y|--debug] [-j|--concurrency ARG] 
               [-l|--files-with-matches] [-r|--recursive] [-n|--line-number]
  Searches for code similar to PATTERN_FILE or PATTERN in SEARCH_DIR.

Available options:
  -W,--verbose             (non grep compat) 'WHAT?' spew debug information
  -Y,--debug               (non grep compat) 'WHY?' just parse the pattern and
                           show, do not search for it
  -j,--concurrency ARG     (non-grep compat) How many threads in parallell
  -l,--files-with-matches  Show only filenames of matched files
  -r,--recursive           IGNORED, always on
  -n,--line-number         IGNORED, always on
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

## What doesn't work and TODO

See Issues tab on github.
