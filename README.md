# Fortran Regular Expression

Fortran Regular Expression (Forgex) is a regular expression engine written entirely in Fortran.

Forgex is managed by [Fortran Package Manager (FPM)](https://fpm.fortran-lang.org/index.html), providing basic processing of regular expression, and as a freely available under the MIT license. 

The engine's core algorithm uses a DFA approach. This choice was focused on runtime performance.

## Features

- Metacharacter
   - `|` Vertical bar, alternation
   - `*` Asterisk, match zero or more
   - `+` Plus, match one or more
   - `?` Question, match zero or one
   - `\`, escape metacharacter
   - `.`, match any character
- Character class
   - character class `[a-z]`
   - inverted character class `[^a-z]`
   - character class on UTF-8 codeset `[α-ωぁ-ん]`
- Range of repetition
   - `{num}`
   - `{,max}`
   - `{min,}`
   - `{min, max}`
- Anchor
   - `^`
   - `$`
- Shorthand
   - `\t`, tab character
   - `\n`, new line character (LF or CRLF)
   - `\r`, return character (CR)
   - `\s`, blank character (white space, TAB, CR, LF, FF, zenkaku space)
   - `\S`, non-blank character
   - `\w`, (`[a-zA-Z0-9_]`)
   - `\W`, (`[^a-zA-Z0-9_]`)
   - `\d`, digit character (`[0-9]`)
   - `\D`, non-digit character (`[^0-9]`)

## Usage
### Build 
It is assumed that you will use the Fortran Package Manager(`fpm`).

Add the following to your project's `fpm.toml`:

```toml
[dependencies]
forgex = {git = "https://github.com/shinobuamasaki/forgex"}
```

### API
When you write `use forgex` at the header on your program, `.in.` and `.match.` operators are introduced.

The `.in.` operator returns true if the pattern is contained in the string.

```fortran
block
   character(:), allocatable :: pattern, str

   pattern = 'foo(bar|baz)'
   str = "foobarbaz"
   print *, pattern .in. str  ! .true.

   str = "foofoo"
   print *, pattern .in. str  ! .false.
end block
```

The `.match.` operator returns true if the pattern exactly matches the string.

```fortran
block
   character(:), allocatable :: pattern, str

   pattern = '\d{3}-\d{4}'
   str = '100-0001'
   print *, pattern .match. str  ! .true.

   str = '1234567'
   print *, pattern .match. str  ! .false.
end block
```

## To do

- [x] UTF-8 Support
- [ ] `pure` API functions. 
- [ ] DFA construction on-the-fly. 
- [ ] Support for CMake and Make. 
- [ ] Parallelization on matching.

## Code Convention

All code contained herein shall be written with a three-space indentation.

## Acknowledgements

For the algorithm of the power set construction method and syntax analysis, I referred to Kondo Yoshiyuki's book.

The implementation of the priority queue was based on [the code written by ue1221](https://github.com/ue1221/fortran-utilities).

## References

1. 近藤嘉雪 (Kondo Yoshiyuki), "定本 Cプログラマのためのアルゴリズムとデータ構造", 1998, SB Creative.
2. [ue1221/fortran-utilities](https://github.com/ue1221/fortran-utilities)