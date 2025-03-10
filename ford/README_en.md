<!-- Readme[EN] version 4.4 -->
[![Tested with GFortran on Ubuntu](https://github.com/ShinobuAmasaki/forgex/actions/workflows/forgex-test-gfortan.yaml/badge.svg?branch=main&event=push)](https://github.com/ShinobuAmasaki/forgex/actions/workflows/forgex-test-gfortan.yaml)
[![Tested with LLVM Flang on Ubuntu](https://github.com/ShinobuAmasaki/forgex/actions/workflows/forgex-test-flang.yaml/badge.svg?branch=main&event=push)](https://github.com/ShinobuAmasaki/forgex/actions/workflows/forgex-test-flang.yaml)
[![Tested with Intel oneAPI](https://github.com/ShinobuAmasaki/forgex/actions/workflows/forgex-test-oneapi.yaml/badge.svg?branch=main)](https://github.com/ShinobuAmasaki/forgex/actions/workflows/forgex-test-oneapi.yaml)

Forgex—Fortran Regular Expression—is a regular expression engine written entirely in Fortran.

This project is managed by [Fortran Package Manager (FPM)](https://fpm.fortran-lang.org/index.html), providing basic processing of regular expression, and as a freely available under the MIT license.
The engine's core algorithm uses a deterministic finite automaton (DFA) approach. This choice have been focused on runtime performance.

## Features


#### Metacharacter
- `|`  Vertical bar for alternation,
- `*`  Asterisk, match zero or more,
- `+`  Plus, match one or more,
- `?`  Question, match zero or one,
- `\`  escape metacharacter,
- `.`  match any character.

#### Character class
- character class `[a-z]`
- inverted character class `[^a-z]`
- character class on UTF-8 codeset `[α-ωぁ-ん]`
- shorthands in character class `[\d]`

Note that inverted class does not match the control characters.

#### Range of repetition
- `{num}`,
- `{,max}`,
- `{min,}`,
- `{min, max}`,
where `num` and `max` must NOT be zero.

To use a literal left curly brace `{`, escape it with a backslash: `\{`.

#### Anchor
- `^`, matches the beginning of a line
- `$`, matches the end of a line

#### Shorthand
- `\t`, tab character
- `\n`, new line character (LF or CRLF)
- `\r`, return character (CR)
- `\s`, blank character (white space, TAB, CR, LF, FF, "Zenkaku" space U+3000)
- `\S`, non-blank character
- `\w`, (`[a-zA-Z0-9_]`)
- `\W`, (`[^a-zA-Z0-9_]`)
- `\d`, digit character (`[0-9]`)
- `\D`, non-digit character (`[^0-9]`)
- `\x..`, `\x{...}` hexadecimal escape sequences, for instance, `\x63` matches `c`.

### Experimental Features

#### Non-UTF-8 Input

**Note: It is the user's responsibility to ensure that input text and the regular expression patterns passed to the API are composed of UTF-8 encoding.**

Version 4.2 adds handling when non-UTF-8 characters are given as input strings. When the processor encounters a non-UTF-8 character, it replaces it byte by byte with U+FFFF and attempts to continue matching. Since this library is primarily focused on UTF-8 string processing, this feature should be considered experimental and preliminary. That is it is intended for simple purposes, such as searching for a pattern that matches ASCII and its class in a text file created with a character encoding that includes ASCII.

## Documentation
The documentation is available in English and Japanese at [https://shinobuamasaki.github.io/forgex](https://shinobuamasaki.github.io/forgex).

## Usage
### Build

Operation has been confirmed with the following compilers:

- GNU Fortran (`gfortran`) v13.2.1
- Intel Fortran Compiler (`ifx`) 2024.0.0 20231017

It is assumed that you will use the Fortran Package Manager(`fpm`).

First of all, add the following to your project's `fpm.toml`:

```toml
[dependencies]
forgex = {git = "https://github.com/shinobuamasaki/forgex"}
```

### Alternative options

#### MacPorts

If you use macOS, you can install this library by using MacPorts with the following command:

```shell
sudo port install forgex
```

In this case, the `.mod` files will be placed in `/opt/local/include/forgex` and the library file will be placed in `/opt/local/lib`,
so to compile your source code, run the following command:

```shell
gfortran main.f90 -I/opt/local/include/forgex -L/opt/local/lib -lforgex
```

If you are using this installation method and want to build using `fpm`, make the following changes to `fpm.toml`:

```toml
[build]
external-modules = [ "forgex" ]
link = [ "forgex" ]
```

Then you can build your program with the following command:

```shell
fpm build --flag "-I/opt/local/include/forgex" --link-flag "-L/opt/local/lib"
```

See also [https://ports.macports.org/port/forgex/details](https://ports.macports.org/port/forgex/details)

#### CMake

If you want to build this library with CMake, execute the following command:
```shell
cd forgex
cmake -S . -B build
cmake --build build
```

Then, you can use codes in `test/` directory to test the library with following command:
```shell
cd build
ctest -C Debug
```

### APIs

When you write `use forgex` at the header on your program, `.in.` and `.match.` operators, `regex` subroutine, and `regex_f` function are introduced.

```fortran
program main
   use :: forgex
   implicit none
```

The `.in.` operator returns true if the pattern is contained in the string.

```fortran
block
   character(:), allocatable :: pattern, str

   pattern = 'foo(bar|baz)'
   str = "foobarbaz"
   print *, pattern .in. str  ! T

   str = "foofoo"
   print *, pattern .in. str  ! F
end block
```

The `.match.` operator returns true if the pattern exactly matches the string.

```fortran
block
   character(:), allocatable :: pattern, str

   pattern = '\d{3}-\d{4}'
   str = '100-0001'
   print *, pattern .match. str  ! T

   str = '1234567'
   print *, pattern .match. str  ! F
end block
```

Note that the `.in.` and `.match.` operators return false for invalid pattern inputs.

The `regex` is a subroutine that returns the substring of a string that matches pattern as `intent(out)` argument.

```fortran
block
   character(:), allocatable :: pattern, str, res
   integer :: length

   pattern = 'foo(bar|baz)'
   str = 'foobarbaz'

   call regex(pattern, str, res)
   print *, res                              ! foobar

   ! call regex(pattern, str, res, length)
         ! the value 6 stored in optional `length` variable.

end block
```

By using the `from`/`to` arugments, you can extract substrings from the given string.

```fortran
block
   character(:), allocatable :: pattern, str, res
   integer :: from, to

   pattern = '[d-f]{3}'
   str = 'abcdefghi'

   call regex(pattern, str, res, from=from, to=to)
   print *, res                   ! def

   ! The `from` and `to` variables store the indices of the start and end points
   ! of the matched part of the string `str`, respectively.

   ! Cut out before the matched part.
   print *, str(1:from-1)        ! abc

   ! Cut out the matched part that equivalent to the result argument of the `regex` subrouine.
   print *, str(from:to)         ! def

   ! Cut out after the matched part.
   print *, str(to+1:len(str))   ! ghi

end block
```

The interface of `regex` subroutine is following:

```fortran
interface regex
   module procedure :: subroutine__regex
end interface

pure subroutine subroutine__regex(pattern, text, res, length, from, to, status, err_msg)
   implicit none
   character(*),              intent(in)    :: pattern, text
   character(:), allocatable, intent(inout) :: res
   integer, optional,         intent(inout) :: length, from, to, status
   character(*), optional,    intent(inout) :: err_msg
```

The list of all `status` values is defined in the source file at src/ast/syntax_tree_error_m.f90.

If you want to the matched character string as the return value of the function,
consider using `regex_f` defined in the `forgex` module.

```fortran
interface regex_f
   module procedure :: function__regex
end interface regex_f

pure function function__regex(pattern, text) result(res)
   implicit none
   character(*), intent(in)  :: pattern, text
   character(:), allocatable :: res
```

#### Validating Regular Expression

Before calling APIs, you can validate a regex pattern using `is_valid_regex` function introduced in version 4.0 and later. The interface of `is_valid_regex` function is following:

```fortran
interface is_valid_regex
   module procedure :: is_valid_regex_pattern
end interfac

pure elemental function is_valid_regex_pattern (pattern) result(res)
   implicit none
   character(*), intent(in)  :: pattern
   logical                   :: res
```

#### UTF-8 String matching

UTF-8 string can be matched using regular expression patterns just like ASCII strings.
The following example demonstrates matching Chinese characters.
In this example, the `length` variable stores the byte length, and in this case there 10 3-byte characters, so the length is 30.

```fortran
block
   character(:), allocatable :: pattern, str
   integer :: length

   pattern = "夢.{1,7}胡蝶"
   str = "昔者莊周夢爲胡蝶　栩栩然胡蝶也"

   print *, pattern .in. str            ! T
   call regex(pattern, str, res, length)
   print *, res                         ! 夢爲胡蝶　栩栩然胡蝶
   print *, length                      ! 30 (is 3-byte * 10 characters)

end block
```

### Command Line Interface Tool

Version 3.2 introduces a command line tool that is called  `forgex-cli` and uses the Forgex engine for debugging, testing, and benchmarking regex matches. It performs matching with commands such as the one shown in below, and outputs the results directly to standard output. [For detailed information, please refer to the documentation.](https://shinobuamasaki.github.io/forgex/page/English/forgex_on_command_line_en.html)

Command:

```shell
forgex-cli find match lazy-dfa '([a-z]*g+)n?' .match. 'assign'
```

If you run it through `fpm run`:

```shell
fpm run forgex-cli --profile release -- find match lazy-dfa '([a-z]*g+)n?' .match. 'assign'
```

Output:

```
            pattern: ([a-z]*g+)n?
               text: 'assign'
         parse time:        46.5us
   compile nfa time:        74.9us
dfa initialize time:        78.4us
        search time:       661.7us
    matching result:         T
 memory (estimated):     10380

========== Thompson NFA ===========
state    1: (?, 5)
state    2: <Accepted>
state    3: (n, 2)(?, 2)
state    4: (g, 7)
state    5: (["a"-"f"], 6)(g, 6)(["h"-"m"], 6)(n, 6)(["o"-"z"], 6)(?, 4)
state    6: (?, 5)
state    7: (?, 8)
state    8: (g, 9)(?, 3)
state    9: (?, 8)
=============== DFA ===============
   1 : ["a"-"f"]=>2
   2 : ["o"-"z"]=>2 ["h"-"m"]=>2 g=>3
   3A: n=>4
   4A:
state    1  = ( 1 4 5 )
state    2  = ( 4 5 6 )
state    3A = ( 2 3 4 5 6 7 8 )
state    4A = ( 2 4 5 6 )
===================================
```

### Notes

- A program built by `gfortran` on Windows and macOC may crash if an allocatable character is used in an OpenMP parallel block.
- If you use the command line tool with PowerShell on Windows, use UTF-8 as your system locale to properly input and output Unicode characters.
- As internal changes to the API related to the addition of the `is_valid_regex` function, the `.in.` and `.match.` operators now return False for invalid pattern input (in versions prior to 3.5 they would terminate processing by executing an `error stop` statement).


## To do

- Character class subtraction: `[a-z--b-d]`
- Add Unicode character class escape sequence: `\p{...}`
- ✅️ Add hexadecimal escape sequence of Unicode: `\x..`
- ✅️ Deal with invalid byte strings in UTF-8
- ✅️ Optimize by literal searching method
- ✅️ Add a CLI tool for debugging and benchmarking
- ✅️ Make all operators `pure elemental` attribute
- ✅️ Publish the documentation
- ✅️ Support UTF-8 basic feature
- ✅️ Construct DFA on-the-fly
- ✅️ Support CMake building
- <s>Parallelize on matching</s>

## Code Convention

All code contained herein shall be written with a three-space indentation.

## Acknowledgements

For the algorithm of the power set construction method and syntax analysis, I referred to Russ Cox's article and Yoshiyuki Kondo's book.
For the algorithm of extracting literals, I refferred to the book of Navarro and Raffnot (2002).
The implementation of the priority queue was based on [the code written by ue1221](https://github.com/ue1221/fortran-utilities).
The idea of applying the `.in.` operator to strings was inspired by kazulagi's one.
The command-line interface design of `forgex-cli` was inspired in part by the package `regex-cli` of Rust language.

## References

1. Russ Cox ["Regular Expression Matching Can Be Simple And Fast"](https://swtch.com/~rsc/regexp/regexp1.html), 2007
2. 近藤嘉雪 (Yoshiyuki Kondo), "定本 Cプログラマのためのアルゴリズムとデータ構造", 1998, SB Creative.
3. [ue1221/fortran-utilities](https://github.com/ue1221/fortran-utilities)
4. Haruka Tomobe (kazulagi), [https://github.com/kazulagi](https://github.com/kazulagi), [his article in Japanese](https://qiita.com/soybean/items/7cdd2156a9d8843c0d91)
5. [rust-lang/regex/regex-cli](https://github.com/rust-lang/regex/tree/master/regex-cli)
6. Gonzalo Navarro and Mathieu Raffnot, "Flexible Pattern Matching in Strings -- Practical On-Line Search Algorithms for Texts and Biological Sequences", 2002, Cambridge University Press

## License
Forgex is as a freely available under the MIT license. See [LICENSE](https://github.com/ShinobuAmasaki/forgex/blob/main/LICENSE).