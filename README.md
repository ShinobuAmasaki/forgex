# Forgex—Fortran Regular Expression

Forgex—Fortran Regular Expression—is a regular expression engine written entirely in Fortran.

Forgex is managed by [Fortran Package Manager (FPM)](https://fpm.fortran-lang.org/index.html), providing basic processing of regular expression, and as a freely available under the MIT license. 
The engine's core algorithm uses a deterministic finite automaton (DFA) approach. This choice was focused on runtime performance.

## Features

- Metacharacter
   - `|` Vertical bar, alternation
   - `*` Asterisk, match zero or more
   - `+` Plus, match one or more
   - `?` Question, match zero or one
   - `\` escape metacharacter
   - `.` matches any character
- Character class
   - character class `[a-z]`
   - inverted character class `[^a-z]`
   - character class on UTF-8 codeset `[α-ωぁ-ん]`
- Range of repetition
   - `{num}`,
   - `{,max}`,
   - `{min,}`,
   - `{min, max}`,
   where `num` and `max` must NOT be zero.
- Anchor
   - `^`, matches the beginning of a line
   - `$`, matches the end of a line
- Shorthand
   - `\t`, tab character
   - `\n`, new line character (LF or CRLF)
   - `\r`, return character (CR)
   - `\s`, blank character (white space, TAB, CR, LF, FF, "Zenkaku" space U+3000)
   - `\S`, non-blank character
   - `\w`, (`[a-zA-Z0-9_]`)
   - `\W`, (`[^a-zA-Z0-9_]`)
   - `\d`, digit character (`[0-9]`)
   - `\D`, non-digit character (`[^0-9]`)

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

### API
When you write `use forgex` at the header on your program, `.in.` and `.match.` operators, and `regex` function are introduced.

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

By using the `from`/`to` arugments, you can extract substrings from the given string.

```fortran
block
   character(:), allocatable :: pattern, str
   integer :: from, to 

   pattern = '[d-f]{3}'
   str = 'abcdefghi'

   print *, regex(pattern, str, from=from, to=to)  ! def
   
   ! The `from` and `to` variables store the indices of the start and end points
   ! of the matched part of the string `str`, respectively.

   ! Cut out before the matched part.
   print *, str(1:from-1)        ! abc

   ! Cut out the matched part that equivalent to the result of the `regex` function. 
   print *, str(from:to)         ! def 

   ! Cut out after the matched part. 
   print *, str(to+1:len(str))   ! ghi

end block
```

The interface of `regex` function is following:

```fortran
function regex (pattern, str, length, from, to) result(res)
   implicit none
   character(*), intent(in) :: pattern, str
   integer, intent(inout), optional :: length, from, to
   character(:), allocatable :: res
```

### UTF-8 String matching

UTF-8 string can be matched using regular expression patterns just like ASCII strings.
The following example demonstrates matching Chinese characters. 
In this example, the `length` variable stores the byte length, and in this case there
10 3-byte characters, so the length is 30.

```fortran
block
   character(:), allocatable :: pattern, str
   integer :: length
   
   pattern = "夢.{1,7}胡蝶"
   str = "昔者莊周夢爲胡蝶　栩栩然胡蝶也"
   
   print *, pattern .in. str            ! T
   print *, regex(pattern, str, length) ! 夢爲胡蝶　栩栩然胡蝶
   print *, length                      ! 30 (is 3-byte * 10 characters)
   
end block
```

## To do

- [ ] Publishing the documentation
- [ ] Dealing with invalid byte strings in UTF-8
- [ ] Literal search optimization
- [ ] Parallelization on matching
- [x] UTF-8 basic support
- [x] DFA construction on-the-fly
- [x] CMake Support

## Code Convention

All code contained herein shall be written with a three-space indentation.

## Acknowledgements

For the algorithm of the power set construction method and syntax analysis, I referred to Russ Cox's article and Kondo Yoshiyuki's book.
The implementation of the priority queue was based on [the code written by ue1221](https://github.com/ue1221/fortran-utilities).
The idea of applying the `.in.` operator to strings was inspired by kazulagi's one.

## References

1. Russ Cox ["Regular Expression Matching Can Be Simple And Fast"](https://swtch.com/~rsc/regexp/regexp1.html), 2007 
2. 近藤嘉雪 (Yoshiyuki Kondo), "定本 Cプログラマのためのアルゴリズムとデータ構造", 1998, SB Creative.
3. [ue1221/fortran-utilities](https://github.com/ue1221/fortran-utilities)
4. Haruka Tomobe (kazulagi), [https://github.com/kazulagi](https://github.com/kazulagi), 
[his article in Japanese](https://qiita.com/soybean/items/7cdd2156a9d8843c0d91)

## License
Forgex is as a freely available under the MIT license. See [LICENSE](https://github.com/ShinobuAmasaki/forgex/blob/main/LICENSE).
