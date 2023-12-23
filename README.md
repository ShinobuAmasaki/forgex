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

Operation has been confirmed with the following compilers:

- GNU Fortran (`gfortran`) v13.2.1
- Intel Fortran Compiler (`ifx`) 2024.0.0 20231017

It is assumed that you will use the Fortran Package Manager(`fpm`).

Add the following to your project's `fpm.toml`:

```toml
[dependencies]
forgex = {git = "https://github.com/shinobuamasaki/forgex"}
```

### API
When you write `use forgex` at the header on your program, `.in.` and `.match.` operators, and `regex` function are introduced.

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

The `regex` is a function that returns the substring of a string that matches pattern.

```fortran
block
   character(:), allocatable :: pattern, str
   integer :: length 

   pattern = 'foo(bar|baz)'
   str = 'foobarbaz'

   print *, regex(pattern, str)              ! foobar
   ! print *, regex(pattern, str, length)    ! the value 6 stored in optional `length` variable.

end block
```

The interface of `regex` function is following:

```fortran
function regex (pattern, str, length) result(res)
   implicit none
   character(*), intent(in) :: pattern, str
   integer, intent(inout), optional :: length
   character(:), allocatable :: res
```
## To do

- [x] UTF-8 Support
- [ ] DFA construction on-the-fly. 
- [ ] Support for CMake and Make. 
- [ ] Parallelization on matching.

## Code Convention

All code contained herein shall be written with a three-space indentation.

## Acknowledgements

For the algorithm of the power set construction method and syntax analysis, I referred to Russ Cox's article and Kondo Yoshiyuki's book.

The implementation of the priority queue was based on [the code written by ue1221](https://github.com/ue1221/fortran-utilities).

The idea of applying the `.in.` operator to strings was inspired by kazulagi's one.

## References

1. Russ Cox ["Regular Expression Matching Can Be Simple And Fast"](https://swtch.com/~rsc/regexp/regexp1.html), 2007 
2.  近藤嘉雪 (Kondo Yoshiyuki), "定本 Cプログラマのためのアルゴリズムとデータ構造", 1998, SB Creative.
3. [ue1221/fortran-utilities](https://github.com/ue1221/fortran-utilities)
4. Haruka Tomobe (kazulagi), [https://github.com/kazulagi](https://github.com/kazulagi), [his article in Japanese](https://qiita.com/soybean/items/7cdd2156a9d8843c0d91)