---
title: Terms of Forgex
author: Amasaki Shinobu
date: 2024-07-12
--- 

# Terms of Forgex

This page provides definitions of terms used in the development of Forgex.

## Contents

- [ASCII](#ascii)
- [Code Set](#code-set)
- [Code Point](#code-point)
- [DFA](#dfa)
- [Disjoin](#disjoin)
- [Lazy DFA](#lazy-dfa)
- [NFA](#nfa)
- [Parsing](#parsing)
- [Powerset Construction](#powerset-construction)
- [Priority Queue](#priority-queue)
- [Segment](#segment)
- [Segment Sorting](#segment-sorting)
- [Subset Construction](#subset-construction)
- [Tape](#tape)
- [Unicode](#unicode)
- [UCS-4](#ucs-4)
- [UTF-8](#utf-8)

## Details

### <span id=ascii>ASCII</span>

ASCII is an acronym for "American Standard Code for Information Interchange", a set of rules
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

In the development of Forgex, we use the UTF-8 codeset, which includes ASCII as a subset, to process regular expression patterns that span the entire character set, where a contiguous subset of UTF-8 is called a Segment. 

See also, [UTF-8](#utf-8), [Code Set](#code-set), [Segment](#segment), [Unicode](#unicode).

### <span id=code-set>Code Set</span>
### <span id=code-point>Code Point</span>

### <span id=dfa>DFA</sapn>
The DFA (deterministic finite automaton) is a theoretical model of computation
in computer science used to represent and manipulate a finite set of states with
deterministic transition, where deterministic transitions is one in which the transition
from state to state is uniquely determined by the input.

An important aspect of to develop a regular expression processor is that the set of
strings that match a regular expression can be computed using a DFA (or an NFA, described below).

The Forgex engine first parses a regular expression into a syntax tree, then constructs an
NFA, which is then converted into an equivalent DFA to perform matching calculations.

The engine uses [powerset construction](#powerset-construction) to construct a DFA.

In its implementation for executing this computation, Forgex defines the `dfa_t` derived-type
using pointers and arrays to represent the directed graph that simulates a DFA.
At this point, a DFA is constructed using the powerset construction method with `dfa_t`,
and the NFA is dynamically converted to a DFA on-the-fly for the input string.
This technique is called "Lazy DFA".

See also, [NFA](#nfa), [Powerset Construction](#powerset-construction), [Lazy DFA](#lazy-dfa).


### <span id=disjoin>Disjoin</span>

<!--
Disjoinは、2つ以上のセグメントについて、交差している部分がある場合、それを解消するためにセグメントsに対して行われる処理である。Forgexの実装では、NFAから部分集合構成法によりDFAを構築する際に、セグメントの交差が存在してはならない。
なぜなら
-->

ref. [(1)](#references)

### <span id=lazy-dfa>Lazy DFA</span>

Unlike traditional DFA construction methods, Lazy DFA is a technique that generates transition as needed by lazy evaluation.
This technique is used to efficiently handle large automaton by computing and storing the transitions from the NFA each time an input is given, reducing memory usage.
Compared to traditional DFA that are pre-calculates everything, for pattens that require a large DFA, such as `a{1,100}*b`, it is possible to avoid pre-calculating the entire DFA, thereby saving memory space.

See also, [DFA](#dfa), [Powerset Construction](#powerset-construction). 


### <span id=nfa>NFA</span>
The NFA (Non-deterministic finite automaton) is a theoretical model of computation in
computer science used to represent and manipulate a finite set of states with non-deterministic
transition. A non-deterministic transition is one in where the transition from state to state
is not uniquely determined for each input. This includes a transition that do not consume
any input string (called ε-transition).

Like the DFA, the NFA can process regular expressions, but due to its non-determinism, 
there is not a single transition from state to state, so a technique called **backtracking**
must be used to effectively simulate it. Although we will not go into details here, engines
that use backtracking in NFA can have a wide range of functionalities, but it is difficult to
achieve high-speed processing for all patterns. In other words, an NFA engine has weaknesses
in some pattern.

Forgex focuses on high runtime performance, which is the main requirement of Fortran users.
Therefore, instead of using NFAs directly for matching, it converts them into eqivalent
DFAs for matching.
The NFA before conversion is represented by the `nfa_t` derived-type.

See also, [DFA](#dfa)

### <span id=powerset-construction>Powerset Construction</span>
### <span id=priority-queue>Priority Queue</span>

### <span id=segment>Segment</span>
A segment is a contigious interval, the subset of a entire character encoding set,
defined by two numbers, a start and an end.
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

See also, [Seguent Sorting](#segment-sorting). 

### <span id=segment-sorting>Segment Sorting</span>
<!--
セグメントのソートは、セグメント集合のDisjoinによって必要とされる処理で、[`forgex_sort_m`](../../module/forgex_sort_m.html)に定義されてソート手続きを、[`fortran_segment_disjoin_m`](../../module/forgex_segment_disjoin.html)内でDisjoinのカーネルが呼び出します。
現在実装されているソートアルゴリズムはバブルソートです。このアルゴリズムが使われている理由はソートする要素数が小さく、処理全体のパフォーマンスへの影響が相対的に小さいからですが、近い将来に挿入ソートに変更することを予定しています。
-->
Sorting segments is a process required by disjoining a set of segments, and the sorting
procedure defined in [`forgex_sort_m`](../../module/forgex_sort_m.html) is called by the
disjoin_kernel in [`forgex_segment_disjoin_m`](../../module/forgex_segment_disjoin_m).

The currently implemented algorithm is **bubble sort**. This algorithm is used because the
number of elements to be sorted is small, and its contribution to the overall performance is
relatively small, but we plan to change it to insertion sort in the near future.

See also, [Disjoin](#disjoin), [Segment](#segment).


### <span id=subset-construciton>Subset Construction</span>
See [Powerset Construction](#powerset-construction).

### <span id=tape>Tape</span>
<!--Forgexの文脈では、Tapeはシーケンシャルなデータアクセスと読み取りヘッダのあるもの、すなわち磁気テープの記憶媒体のようなものへアナロジーです。
これは以下の`tape_t`派生型として構文解析モジュール（[`forgex_syntax_tree_m`](../../module/forgex_syntax_tree_m.html)）で定義されています。
この型は内部に入力パターン文字列全体（巻かれた磁気テープ）と添字番号（読み取りヘッダー）の情報を含んでおり、プログラマーはこの型の型束縛手続を介して現在読み取る文字列とトークンを保持します。
-->
In the Forgex context, a Tape mimics a storage medium (such as a magnetic tape) with sequential data access and a read header.
It is defined in the syntax analysis module ([`forgex_syntax_tree_m`](../../module/forgex_syntax_tree_m.html)) as the [`tape_t`](../../type/tape_t.html) derived type. 
This type contains information about the entire input pattern string (rolled magnetic tape) and the index number (read header), and the programmer can use the currently read character and tokens through the type binding procedure of this type. 

### <span id=unicode>Unicode</span>
<!--
Unicodeは、世界中の文字と記号を統一的に扱うための標準規格であり、これにより異なる言語やシステム間のテキストが一貫して表示され、交換可能であることを実現している。Unicodeは、文字ごとに一意の番号（コードポイント）を割り当てることで、文字の識別と処理を簡素化している。Unicodeのコードポイントは、例えばラテン文字の"A"はU+0041、日本語の"あ"はU+3042の番号が割り当てられている。
Unicodeの文字は、UTF-8などの符号化方式を使ってバイトストリングとしてエンコードされてシステムに処理される。
-->
See also, [UTF-8](#utf-8)

### <span id=ucs-4>UCS-4</span>
UCS-4, or the nearly equivalent UTF-32, is a fixed-length encoding scheme that assign a 32-bit(4 bytes) binary string to each Unicode code point.

### <span id=utf-8>UTF-8</span>
<!--
UTF-8は、1バイトから4バイトの可変長でUnicode文字を対応づける文字符号化スキームである。
ASCII文字と互換性を保つためにASCII部分は1バイト、その他の文字は2-4バイトで表現される。
-->
## <span id=references>Refereces</span>
1. [How to implement regular expression NFA with character ranges? - Stack Overflow](https://stackoverflow.com/questions/20767047/how-to-implement-regular-expression-nfa-with-character-ranges), 2013