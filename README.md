# Isocode
![Docker Image CI](https://github.com/ikkeps/isocode/workflows/Docker%20Image%20CI/badge.svg?branch=master)

Search Perl 5 sources by given code example.

You can give it file or string as a pattern. Command line interface is compatible with `grep`, so in common cases you can replace `grep` with `isocode`.
Binary is build for Linux and completely static, so no need to install any dependencies.

See `--help` for all supported options.

Rules of matching:

* All brackets should have pair in pattern. E.g. you can not search for ` if ( `. Sorry :(
* ` "abc" ` == ` 'abc' ` == `q(abc)` == `qq(abc)`. Even `"123"` == `123`
* ` m/[a-z]/ ` == ` /[a-z]/ ` == ` "[a-z]" ` . But if there are any flags - it should be always a regexp.
* `qw( 1 2  3 )` == `qw(1 2 3)` == `(1,2,3)` == `("1", 2, "3")` ...
* Trailing separators ignored: `(1,2,3,)` == `(1,2,3)` and `{ a(); b(); }` == ` { a(); b() } `
* Variable names can be differrent! But must be consistent: ` $a + $a ` == ` $b + $b `, but ` $a + $b ` != ` $c + $c `
* Whitespace, newlines and comments are ignored.
* *BETA*: Wildcard ` *** ` matches anything till the end of the block. You can use it like ` if (! ***) {die *** }`. (dont forget to surround it with spaces, so parser dont get confused) . #11

## Run

```
$ ./isocode -nr -e 'my ( $type, %args ) = @_; my $a = {}; bless( $a, $type);' ~/tmp/otrs/
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

## Why

https://www.youtube.com/watch?v=JtHi6bSZX4E

For fun. Maybe it will help someone.

"isocode" means "isomorphic code".

## Build

I just gave up setting up ghc no my machine to build static binary, so its built in docker.

```
# ./build-in-docker.sh
# ./isocode --help
isocode - search source tree by Perl code sample

Usage: isocode (PATTERN | (-e|--regexp PATTERN) | (-f|--file PATTERN_FILE))
               SEARCH_DIR [-W|--verbose] [-Y|--debug] [-l|--files-with-matches] 
               [-r|--recursive] [-n|--line-number]
  Searches for code similar to PATTERN_FILE or PATTERN in SEARCH_DIR.

Available options:
  -W,--verbose             (non grep compat) 'WHAT?' spew debug information
  -Y,--debug               (non grep compat) 'WHY?' just parse the pattern and
                           show, do not search for it
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

TLDR: Basically, its three parser generators (one for command line arguments) stiched together.

## What (doesn't) work and TODO

See Issues tab on github. Both already implemented and still TBD should be there.
