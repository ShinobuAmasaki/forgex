---
title: Terms related to Forgex
author: Amasaki Shinobu
date: 2024-07-13
--- 

# Terms related to Forgex

This page provides details of terms used in the development of Forgex.

## Contents

- [ASCII](#ascii)
- [Code Point](#code-point)
- [DFA](#dfa)
- [Disjoin](#disjoin)
- [Lazy DFA](#lazy-dfa)
- [NFA](#nfa)
- [Powerset Construction](#powerset-construction)
- [Segment](#segment)
- [Segment Sorting](#segment-sorting)
- [Subset Construction](#subset-construction)
- [Tape](#tape)
- [Unicode](#unicode)
- [UCS-4](#ucs-4)
- [UTF-8](#utf-8)

## Details


### <span id=ascii>ASCII</span>
**ASCII** is an acronym for "American Standard Code for Information Interchange", a set of rules
established in 1963 that defines the relationship between the numbers 0 to 127 and which
letters and symbols correspond to them.
The first 32 characters (0-31 in decimal, and so on) are reserved as control characters,
and the last 96 characters (32-127) are printable characters.
The printable characters contain the Latin alphabet used in the United States, with numbers 65-90
corresponding to uppercase letters A-Z, and numbers 97-122 corresponding to lowercase letter a-z.
The others are symbols such as "$", "#", and "|".

In Fortran, you can obtain this correspondence using the intrinsic procedures `char()` and `ichar()`.
For example, if you give the `char` argument the number 70, it will return the letter 'F',
and conversely, if you give the `ichar` argument the letter 'o', it will return the integer 111.

In the development of Forgex, we use the UTF-8 codeset, which includes ASCII as a subset, to process
regular expression patterns that span the entire character set, where a contiguous subset of UTF-8
is called a Segment. 

See also, [Segment](#segment), [Unicode](#unicode), [UTF-8](#utf-8).


### <span id=code-point>Code Point</span>
A **code point** (also known as **code position**) is a paricular position in table that has a scripts,
symbols, emojis and control character assigned to it.

In Unicode, code points are expressed as a hexadecimal number following the U+ prefix,
and range from U+0000 to U+10FFFF.
For example, the code point of the Latin letter 'A' is U+0041.
Similarly, the kanji character '雨' corresponds to U+96E8, and the emoji '👍' corresponds to U+1FF4D.

Forgex represents Unicode code points as integer and defines the `char_utf8` and `ichar_utf8` procedures
in the `forgex_utf8_m` module to convert to and from the corresponding UTF-8 encoding characters.

See also, [Unicode](#unicode), [UTF-8](#utf-8).

### <span id=dfa>DFA</sapn>

The **DFA** (deterministic finite automaton) is a theoretical model of computation
in computer science used to represent and manipulate a finite set of states with
deterministic transitions, where a deterministic transition is one in which the transition
from state to state is uniquely determined by the input.

An important aspect of to develop a regular expression processor is that the set of
strings that match a regular expression can be computed using a DFA (or an NFA, described below).

The Forgex engine first parses a regular expression into a syntax tree, then constructs an
NFA, which is then converted into an equivalent DFA to perform matching calculations.
The engine uses the powerset construction method to construct a DFA.
Here, the NFA is dynamically converted to a DFA on-the-fly for input character.
This technique is called Lazy DFA construction.
In its implementation for executing this computation, Forgex defines the `dfa_t` derived-type
using pointers and arrays to represent the directed graph that simulates a DFA.


See also, [NFA](#nfa), [Powerset Construction](#powerset-construction), [Lazy DFA](#lazy-dfa).


### <span id=disjoin>Disjoin</span>

In the development of Forgex, **disjoin** refers to a a set of operations that are performed on
a set of segments to eliminate crossing segments between multiple segments.

As a premise, Forgex represents a set of inputs that share a common transition as a segment.
In this case, if crossing segments are contained in the set, the Forgex implementation of
powerset construction cannot construct a DFA equivalent to the original NFA.
Therefore, we need to perform a disjoin operation to convert the set of crossing segments
into a set of non-crossing segments by spliting them at their crossing point.

The disjoin operation is defined as public procedures in the `forgex_segment_disjoin_m` module,
and in particular the `disjoin_kernel` procedure within it plays an important role.

See also, [Segment](#segment), [``forgex_segment_disjoin_m`](../../module/forgex_segment_disjoin_m.html),  ref.[(1)](#references).


### <span id=lazy-dfa>Lazy DFA</span>

Unlike traditional DFA construction methods, **Lazy DFA** is a technique that generates
transition as needed by lazy evaluation.
This technique is used to efficiently handle large automaton by computing and storing
the transitions from the NFA each time an input is given, reducing memory usage.
Compared to traditional DFA that are pre-calculates everything, for pattens that require
a large DFA, such as `a{1,100}*b`, it is possible to avoid pre-calculating the entire DFA,
thereby saving memory space.

See also, [DFA](#dfa), [Powerset Construction](#powerset-construction). 


### <span id=nfa>NFA</span>

The **NFA** (Non-deterministic finite automaton) is a theoretical model of computation in
computer science used to represent and manipulate a finite set of states with non-deterministic
transition. A non-deterministic transition is one in where the transition from state to state
is not uniquely determined for each input. This includes a transition that do not consume
any input string (called ε-transition).

Like the DFA, the NFA can process regular expressions, but due to its non-determinism, 
there is not a single transition from state to state, so a technique called **backtracking**
must be used to effectively simulate it. Although we will not go into details here, engines
that use backtracking in NFA can have a wide range of functionalities, but it is difficult to
achieve high-speed processing for all patterns. In other words, an NFA engine has weaknesses
in some kind of patterns.

Forgex focuses on high runtime performance, which is the main requirement of Fortran users.
Therefore, instead of using NFAs directly for matching, it converts them into eqivalent
DFAs for matching.
The NFA before conversion is represented by the `nfa_t` derived-type.
For the details of that conversion, you can see the Powerset Construction section.

See also, [DFA](#dfa), [Powerset Construction](#powerset-construction).


### <span id=powerset-construction>Powerset Construction</span>
The **powerset construction** method, also known as the **subset construction** method, is a process
to convert an NFA into a DFA.
This method allows us to convert automata with non-deterministic properties into equivalent DFAs,
i.e. it accepts the same input strings.

This approach is powerful in that it gives us a deterministic state machine.
It has drawbacks, however, as the potentially exponential growth in the number of DFA states
constructed by the transformation.
This problem is a kind of problem called combinatiorial explosion.
Fortunately, Forgex version 2.0 and later introduces a lazy DFA construction method that can dynamically
generate a DFA state for the input characters, so we don't need to worry about this problem here.

cf. <a href="https://en.wikipedia.org/wiki/Powerset_construction" target="_blank" rel="noopener noreferrer">Powerset construction - Wikipedia</a>

cf. <a href="https://en.wikipedia.org/wiki/Combinatorial_explosion" target="_blank" rel="noopener noreferrer">Combinatorial explosion - Wikipedia</a>

See also, [Lazy DFA](#lazy-dfa).


### <span id=segment>Segment</span>
A **segment** is a contiguous interval, the subset of an entire character encoding set,
defined by two numbers: a start and an end.
Assigning each input single character to a transition in the simulation of a state machine would consume
a lot of memory, especially when processing character classes, so Forgex uses a method of associating
such intervals with a transition.
This approach also introduces new problems; see the Disjoin explanation for more details.

In Forgex's segment implementation, the `segment_t` derived-type is defined as follows:

```fortran
   type, public :: segment_t
      integer(int32) :: min = UTF8_CODE_EMPTY ! = 0
      integer(int32) :: max = UTF8_CODE_EMPTY ! = 0
   contains
      procedure :: validate => segment_is_valid
   end type
```

The `segment_t` type has two component of `min` and `max`, and a type-bound procedures, `validate`.
The `min` is the smallest number of characters in the interval, and max is the largest number.
The `validate` procedure checks whether the `min` component is smaller than or equal to `max`.
If `min` and `max` are equal, the segment refers to exactly one character.

See also, [Disjoin](#disjoin), [Seguent Sorting](#segment-sorting). 


### <span id=segment-sorting>Segment Sorting</span>

**Sorting segments** is a process required by disjoining of a set of segments, and the sorting
procedure defined in `forgex_sort_m` is called by the disjoin_kernel in `forgex_segment_disjoin_m`.
The currently implemented algorithm is bubble sort. This algorithm is used because the
number of elements to be sorted is small, and its contribution to the overall performance is
relatively minor.
However, we plan to change it to insertion sort in the near future.

See also, [Disjoin](#disjoin), [Segment](#segment), [`forgex_sort_m`](../../module/forgex_sort_m.html),
[`forgex_segment_disjoin_m`](../../module/forgex_segment_disjoin_m).


### <span id=subset-construciton>Subset Construction</span>

See [Powerset Construction](#powerset-construction).


### <span id=tape>Tape</span>

In the Forgex context, a **Tape** mimics a storage medium (such as a magnetic tape) with sequential data access
and a read header.
It is defined in the syntax analysis module (`forgex_syntax_tree_m`) as the `tape_t` derived type. 
This type contains information about the entire input pattern string (like a rolled magnetic tape) and
the index number (read header).
The developers of Forgex can use the currently read character and tokens through the type-bound procedure. 

See also, ([`forgex_syntax_tree_m`](../../module/forgex_syntax_tree_m.html)), [`tape_t`](../../type/tape_t.html)


### <span id=unicode>Unicode</span>
**Unicode** is one of the character encoding standards, which enables consistent representation and handling of text
across different languages and platforms.
It assigns a unique number (code point) to every character and symbol, covering a wide range of
scripts, symbols, and even emojis.
Unicode characters are encoded using common encoding schemes like UTF-8, UTF-16, and UTF-32 into byte strings,
ensuring compatibility across different platforms.

Even in Fortran programming, many compilers allow us to handle Unicode characters by setting the terminal and
source file encoding to UTF-8.

@note In the case of Microsoft's Windows operating system, the system's standard character encoding
may not be UTF-8, so users may need to change the settings appropriately.

See also, [Code Point](#code-point), [UTF-8](#utf-8)


### <span id=ucs-4>UCS-4</span>
**UCS-4** (Universal Coded Character Set 4), or the nearly equivalent UTF-32 (defined in ISO/IEC 10646),
is a fixed-length encoding scheme that assigns a 32-bit (4 bytes) binary string to each Unicode code point.
In some Fortran 2003 conforming compilers, we can use these fixed-length 4-byte characters by specifying the
`kind` type parameter in a character type declaration as the return value of `selected_char_kind('ISO_10646')`.
For example, GNU Fortran Compiler supports this.
Forgex currently does not provide support for UCS-4 string processing. 

cf. <a href="https://en.wikipedia.org/wiki/UTF-32" target="_blank" rel="noopener noreferrer">UTF-32 - Wikipedia</a>

See also, [Unicode](#unicode), [UTF-8](#utf-8)


### <span id=utf-8>UTF-8</span>
**UTF-8** (UCS Transformation Format 8, or Unicode Transformation Format-8) is a character encoding
scheme that maps Unicode characters to binary strings of variable length, from 1 to 4 bytes.
To maintain compatibility with ASCII characters, the ASCII characters part is represented in 1 byte, and other
characters are represented in 2-4 bytes.
Forgex processes UTF-8 encoded character strings using the procedures defined in the `forgex_utf8_m` module.

See also, [`forgex_utf8_m`](../../module/forgex_utf8_m.html).


## <span id=references>Refereces</span>
1. <a href="https://stackoverflow.com/questions/20767047/how-to-implement-regular-expression-nfa-with-character-ranges" target="_blank" rel="noopener noreferrer">How to implement regular expression NFA with character ranges? - Stack Overflow</a>, 2013
2. <a href="https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764?u=amasaki203" target="_blank" rel="noopener noreferrer">Using Unicode Characters in Fortran - Fortran-lang Discourse</a>
