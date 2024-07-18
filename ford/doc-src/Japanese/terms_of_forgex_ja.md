---
title: Forgexの用語
author: 雨崎しのぶ
date: 2024-07-18
---

# Forgexの開発における用語

このページには、Forgexの開発に関わる用語についての解説が含まれています。

## 目次
- [ASCII](#ascii)
- [コードポイント](#code-point)
- [DFA](#dfa)
- [Disjoin](#disjoin)
- [Lazy DFA](#lazy-dfa)
- [NFA](#nfa)
- [冪集合構成法](#powerset-construction)
- [セグメント](#segment)
- [セグメントのソート](#segment-sorting)
- [部分集合構成法](#subset-construction)
- [テープ](#tape)
- [Unicode](#unicode)
- [UCS-4](#ucs-4)
- [UTF-8](#utf-8)

## 詳細

### <span id=ascii>ASCII</span>

ASCII（American Standard Code for Information Interchange）は、1963年に制定された文字符号化に関する規則で、0から127の数字とそれらに対応する文字および記号の関係を定義しています。最初の32文字（10進数の0から31）は制御文字として予約されており、最後の96文字（32から127）は印刷可能（Printable）な文字です。印刷可能文字には、アメリカで使用されているラテン文字が含まれており、数字65～90は大文字のA~Zに対応し、数字97～122は小文字のa～zに対応します。その他は「$」、「#」、「|」などの記号です。

Fortranでは、組込み手続`char()`や`ichar()`を使用してこの対応関係を取得することができます。例えば`char`の引数に数値70を指定すると文字「F」が返され、逆に`ichar`の引数に文字「o」を指定すると、整数111が返されます。

Forgexの開発では、ASCIIを部分集合として含むUTF-8コードセットを使用して、文字集合全体にわたる正規表現パターンを処理します。UTF-8の連続したサブセットは、その数値に対応する「セグメント」を定義し、これを使用してUTF-8文字の処理を実現しています。

cf.  [セグメント](#segment)、 [Unicode](#unicode)、 [UTF-8](#utf-8)

### <span id=code-point>コードポイント（Code Point）</span>

コードポイント（コードポジションとも呼ばれる）は文字、記号、絵文字及び制御文字が割り当てられている表の中の特定の位置を指します。

Unicodeでは、コードポイントは「U+」の接頭辞に続く16進数で表現され、その範囲はU+0000からU+10FFFFです。例えば、ラテン文字「A」のコードポイントはU+0041です。同様に漢字「雨」はU+96E8に対応し、絵文字「👍」は U+1FF4D に対応します。

ForgexはUnicodeコードポイントを整数として表現し、`forgex_utf8_m`モジュールで`char_utf8`及び`ichar_utf8`の手続を定義して、コードポイントとそれに対応するUTF-8文字との間での変換を行います。

cf.  [Unicode](#unicode)、 [UTF-8](#utf-8)

### <span id=dfa>DFA</span>

決定性有限オートマトン（Deterministic Finite Automaton、 **DFA**）は、決定論的遷移を持つ有限の状態集合について、表現及び操作するために使用される計算機科学における計算の理論モデルです。決定論的遷移とは、状態から状態への遷移が入力文字によって一意に決定されるものを指します。

正規表現処理系の開発における重要な点は、正規表現に一致する文字列集合はDFA（または後述のNFA）を使用して計算することができることです。

Forgexの正規表現エンジンは、まず正規表現のパターンから構文木を作成し、次にNFAを構築します。そしてそのNFAが等価なDFAに変換されて、マッチングの計算が行われます。このとき、構築されたNFAから冪集合構成法（後述）を使用してDFAを構築しますが、現在のバージョンのForgexでは、NFAと入力文字列に対して遅延評価（つまり入力がある前にDFA全体を構築しない）を行い、DFAを構築していきます。この手法はLazy DFAと呼ばれています。この計算を実行するためのForgexの実装では、DFAをシミュレートするラベル付き有向グラフを表すポインタと配列を使用して`dfa_t`派生型を定義しています。

cf. [NFA](#nfa)、 [冪集合構成法](#powerset-construction)、 [Lazy DFA](#lazy-dfa)

### <span id=disjoin>Disjoin</span>

Forgexの開発において、Disjoinとは、複数のセグメントの間で、互いに交差するセグメントをなくすために、一連のセグメントに対して行われる操作をさす。

前提として、Forgexは共通の遷移を共有する入力の集合をセグメントとして表現しています。この場合、交差するセグメントがその集合に含まれていると、Forgexの冪集合構成法の実装では、元のNFAと等価なDFAを構築することはできません。したがって、交差するセグメントの集合を交差点で分割することにより、交差しないセグメントの集合に変換する分割の操作を実行する必要があります。

Disjoinの操作は、`forgex_segment_disjoin_m`モジュール内の公開手続として定義されており、特にその中で`disjoin_kernel`手続が重要な役割を果たします。

cf. [セグメント](#segment)、 [``forgex_segment_disjoin_m`](../../module/forgex_segment_disjoin_m.html)、

### <span id=lazy-dfa>Lazy DFA</span>

Lazy DFAは、通常のDFA構築手法とは異なり、遅延評価によって必要に応じて遷移と遷移先を生成する手法です。この手法は、入力が与えられるたびにNFAからの遷移を計算して保存することで大規模なオートマトンを効率的に処理するために使用され、記憶領域の消費量を削減します。すべてのDFA状態を事前に計算する通常のDFA構成の場合と比較して、`a{1、100}*b`などの大規模なDFAを必要とするパターンの場合についてDFA全体の事前計算を回避できるため、メモリスペースを節約することができます。

cf. [DFA](#dfa)、 [冪集合構成法](#powerset-construction). 

### <span id=nfa>NFA</span>

非決定性有限オートマトン（Non-deterministic Finite Automaton、**NFA**）は、非決定的遷移を伴う有限の状態集合について、表現及び操作するために使用される計算機科学における計算の理論モデルです。非決定的遷移とは、状態から状態への遷移が入力ごとに一意に決定されない遷移です。これには入力文字列を消費しない遷移（ε遷移）も含まれます。

DFAと同様に、NFAは正規表現を処理できますが、効果的にシミュレートするためにはバックトラックと呼ばれる手法を使用する必要があります。ここでは詳細に説明できませんが、NFAについてバックトラックを利用した正規表現処理系は幅広い機能を搭載できる一方で、すべてのパターンで高速な処理を実現することは困難です。つまり、NFAによるエンジンには苦手なパターンというものが存在しています。

Forgexは、多くのFortranユーザーが主眼においている要件である、高い実行時のパフォーマンスに重点をおいています。したがって、NFAをマッチングに直接使うのではなく、NFAと同等のDFAに変換してマッチングを行います。変換前のNFAは`nfa_t`派生型として定義されています。その変換の詳細については、後述の「冪集合構成法」を参照してください。

cf. [DFA](#dfa)、 [冪集合構成法](#powerset-construction)

### <span id=powerset-construction>冪集合構成法（Powerset Construciton Method）</span>

冪集合構成法（Powerset Construction Method）または部分集合構成法（Subset Construction Method）は、NFAをDFAをに変換する処理です。この方法を使用すると非決定的性質を持つオートマトンをそれと等価な、つまり同じ入力文字列を受理するDFAに変換することができます。

このアプローチは、決定性状態機械を構築できるという点で強力なものですが、しかしながら、変換によって構築されるDFA状態の数が指数関数的に増加する可能性があるという欠点を持ちます。この問題は組合せ爆発と呼ばれる問題の一種です。Forgexのバージョン2.0以降では入力文字に対応するDFA状態を動的に生成できるLazy DFAが導入されているので、この問題について心配する必要はありません。

cf. [部分集合構成法 - Wikipedia](https://ja.wikipedia.org/wiki/%E9%83%A8%E5%88%86%E9%9B%86%E5%90%88%E6%A7%8B%E6%88%90%E6%B3%95)、[組合せ爆発- Wikipedia](https://ja.wikipedia.org/wiki/%E7%B5%84%E5%90%88%E3%81%9B%E7%88%86%E7%99%BA)

### <span id=segment>セグメント（Segment）</span>

セグメント（segment）とは、文字集合全体の部分集合であり連続した区間として、開始点と終了点の2つの数字で定義されます。状態機械をシミュレートにおいて、単一の入力文字を計算するべき遷移に割り当てると、（特に文字クラスおよび否定クラスを処理する場合に）大量のメモリを消費するため、Forgexは文字集合の部分的な区間を遷移に関連付ける方法を使用して、メモリの消費を低減しています。ただし、このアプローチを導入することによって新たな問題が生じることにも注意してください。その詳細についてはDisjoinの説明を参照してください。

Forgexのセグメントの実装では、`segment_t`派生型として次のように定義されています。

```fortran
   type、 public :: segment_t
      integer(int32) :: min = UTF8_CODE_EMPTY ! = 0
      integer(int32) :: max = UTF8_CODE_EMPTY ! = 0
   contains
      procedure :: validate => segment_is_valid
   end type
```

`segment_t`型には、`min`と`max`の2つの成分と型束縛手続の`validate`が含まれます。`min`は区間内で最小のコードポイント値、`max`は最大のコードポイント値を保持します。手続`validate`は成分`min`が成分`max`以下であるかどうかを確認します。`min`と`max`が等しい場合、そのセグメントはただ1文字のみを表現します。

cf. [Disjoin](#disjoin)、 [セグメントのソート](#segment-sorting) 

### <span id=segment-sorting>セグメントのソート</span>

セグメントのソートには、セグメントの集合をDisjoinな状態に再構築するために必要な処理で、`forgex_sort_m`モジュールで定義されたソート手順は、`forgex_segment_disjoin_m`モジュールの`disjoin_kernel`手続によって呼び出されます。現在実装されているアルゴリズムはバブルソートです。このアルゴリズムが使用されているのは、ソートされる要素の数が少なく、実行時間に対するこの処理の寄与が比較的小さいためです。ただし、近い将来に挿入ソートに変更することを予定しています。

cf. [Disjoin](#disjoin)、 [セグメント](#segment)、 [`forgex_sort_m`](../../module/forgex_sort_m.html)、 [`forgex_segment_disjoin_m`](../../module/forgex_segment_disjoin_m).

### <span id="subset-construction">部分集合構成法（Subset Construction Method）</span>

[冪集合構成法](#powerset-construction)を参照してください。

### <span id=tape>テープ（Tape）</span>

Forgexの実装において、テープ（tape）とは、シーケンシャルなデータアクセスと読み取りヘッダーを備えたストレージ（磁気テープなど）に例えて、それを模倣した派生型を使用しています。これは構文解析モジュール（`forgex_syntax_tree_m`）において`tape_t`派生型として定義されています。この型には、入力パターンの文字列全体（巻かれた磁気テープの例え）とインデックス番号（読み取りヘッダーの例え）に関する情報が含まれています。Forgexの開発者は、現在読み込まれている文字とトークンを、それの型束縛手続を通じて使用することができます。

cf. forgex_syntax_tree_m`](../../module/forgex_syntax_tree_m.html)、 [`tape_t`](../../type/tape_t.html)

### <span id=unicode>Unicode</span>

Unicodeは文字符号化の標準規格の一つであり、これを使用することで、さまざまな言語やプラットフォーム間でテキストの一貫した表現と処理が可能となる。すべての文字と記号に一意の番号（コードポイント）を割り当てて、広範囲の文字、記号、さらに絵文字をカバーしている。Unicode文字は、UTF-8、UTF-16、UTF-32などの共通の符号化方式を使用してバイト列にエンコードされ、様々なプラットフォーム間での互換性が確保されています。

@note MicrosoftのWindowsオペレーティングシステムの場合、システムの標準の文字コードがUTF-8でない場合があるため、ユーザーが設定を適切な変更を行う必要があるかもしれません。

cf. [コードポイント](#code-point)、 [UTF-8](#utf-8)

### <span id=ucs-4>UCS-4</span>

UCS-4（Universal Coded Character Set 4）もしくはほぼ同等のUTF-32（ISO/IEC 10646で定義されたされている）は、それぞれのUnicodeのコードポジションに32ビット（4バイト）のバイナリ列を割り当てる固定長の符号化方式です。Fortran 2003準拠のコンパイラの一部では、文字列型の宣言において型パラメーター`kind`を`selected_char_kind('ISO_10646')`の戻り値に指定することで、この固定長4バイト文字を使用することができる。例えば GNUのFortranコンパイラはこれをサポートしています。

Forgexは現在のところ、UCS-4文字列の処理をサポートしていません。

cf. [Unicode](#unicode)、 [UTF-8](#utf-8)、<a href="https://ja.wikipedia.org/wiki/UTF-32" target="_blank" rel="noopener noreferrer">UTF-32 - Wikipedia</a>

### <span id=utf-8>UTF-8</span>

UTF-8（UCS Transformation Format 8、または Unicode Transformation Format-8）は、Unicode文字を1バイトから4バイトの可変長バイト列に対応させる文字符号化の方式の1つです。ASCII文字との互換性を維持するために、ASCII文字の部分は1バイトで表現され、その他の文字は2バイトから4バイトで表現されます。Forgexは`forgex_utf8_m`モジュールで定義された手続を使用して、UTF-8で符号化された文字列を処理します。

cf. [`forgex_utf8_m`](../../module/forgex_utf8_m.html)

## <span id=references>参考文献</span>

1. <a href="https://stackoverflow.com/questions/20767047/how-to-implement-regular-expression-nfa-with-character-ranges" target="_blank" rel="noopener noreferrer">How to implement regular expression NFA with character ranges? - Stack Overflow</a>、 2013
2. <a href="https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764?u=amasaki203" target="_blank" rel="noopener noreferrer">Using Unicode Characters in Fortran - Fortran-lang Discourse</a>