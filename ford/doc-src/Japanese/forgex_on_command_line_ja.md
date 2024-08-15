---
title: CLIツール
author: 雨崎しのぶ
date: 2024-08-13
---

# コマンドラインインターフェース

## 概要

正規表現のテストケースのいくつかの例は`test/`ディレクトリに配置されており、`fpm test`コマンドで簡単に実行することができます。

これらに含まれるものの他に、正規表現のマッチングを確認したい場合には、バージョン3.2から導入されたコマンドライン・インターフェースのツール`forgex-cli`が利用可能です。
例えば、`((a|b)*)*`と`ababab`のマッチングをテストしたい場合には、次のコマンドを実行すると以下のような出力が得られます。

<div class="none-highlight-user">

```
% forgex-cli find match lazy-dfa '((a|b)*)*' .match. 'ababab'
            pattern: ((a|b)*)*
               text: ababab
         parse time:        32.6μs
   compile nfa time:        49.5μs
dfa initialize time:        55.7μs
  dfa matching time:       643.7μs
    matching result:         T
 memory (estimated):      6781

========== Thompson NFA ===========
state    1: (?, 3)
state    2: <Accepted>
state    3: (?, 5)(?, 2)
state    4: (?, 3)
state    5: (["a"-"b"], 6)(?, 4)
state    6: (?, 5)
=============== DFA ===============
   1A: ["a"-"b"]=>2
   2A: ["a"-"b"]=>2
state    1A = ( 1 2 3 4 5 )
state    2A = ( 2 3 4 5 6 )
===================================
```
</div>

コマンドラインの出力は、上部の実行時間などを示す表と、下部のオートマトンの状態と遷移を表す行から構成されます。
このツールを使用して、正規表現マッチングのベンチマークや、デバッグおよびテストを行うことができます。

現在のところ、`find`と`debug`のコマンドが利用可能です。また、`forgex-cil`のコマンドは`fpm run`から実行することも可能です。

<div class="none-highlight-user">

```
% fpm run forgex-cli --profile release -- find match forgex '((a|b)*)*' .match. 'ababab'
...
libforgex.a                            done.
forgex-cli.f90                         done.
forgex-cli                             done.
[100%] Project compiled successfully.
pattern: ((a|b)*)*
   text: ababab
   time:       487.1us
 result:         T
```

</div>

## `forgex-cli debug`コマンド

以下に、`forgex-cli debug`コマンドのヘルプメッセージを示します。

<div class="none-highlight-user">

```
% forgex-cli debug --help
Prints the debug representation provided by Forgex.

USAGE:
   forgex-cli debug <command> ...

COMMANDS:
   ast           Print the debug representation of an AST.
   thompson      Print the debug representation of a Thompson NFA.
```

</div>

`debug`コマンドでは、与えられたパターンについて、抽象構文木（AST）または非決定性有限オートマトン （NFA）を出力します。

以下は`ast`サブコマンドを使用して正規表現パターンから構築されたASTを出力する例です。

<div class="none-highlight-user">

```
% forgex-cli debug ast "((a|b)*)*"
Project is up to date
        parse time:        29.5us 
memory (estimated):       829
(closure(closure(or "a" "b")))
```

</div>

一方、ASTから変換されたNFAの構造を知りたい場合には、次のように`thompson`サブコマンドを実行します。

<div class="none-highlight-user">

```
% forgex-cli debug thompson "((a|b)*)*"
Project is up to date
        parse time:        26.5us 
  compile nfa time:        42.4us 
memory (estimated):      6271

========== Thompson NFA ===========
state    1: (?, 3)
state    2: <Accepted>
state    3: (?, 5)(?, 2)
state    4: (?, 3)
state    5: (["a"-"b"], 6)(?, 4)
state    6: (?, 5)

Note: all segments of NFA were disjoined with overlapping portions.
===================================
```

</div>

このコマンドラインの出力では、それぞれのNFA状態について、左辺に状態番号と右辺にNFA遷移がセットで記述されています。

- (["a"-"b"], 6)`という遷移は「文字コード表でaからbの範囲の文字が入力された場合に第6状態へ遷移する」という意味になります。
- `(?, 3)`のような、入力文字が`?`となっているものは、ε（イプシロン）遷移と呼ばれるもので、入力文字列を消費せずに遷移可能であることを示しています。この例では受理状態を除いてε遷移が各NFA状態に含まれています。

## `forgex-cli find`コマンド

以下に`find`コマンドと`match`サブコマンドのヘルプメッセージの出力を示します。

<div class="none-highlight-user">

```
% forgex-cli find --help
Executes a search.

USAGE:
   forgex-cli find <command> ...

COMMANDS:
   match         Search for full matches.
```

</div>
<div class="none-highlight-user">

```
% forgex-cli find match --help
Executes a search for full matches.

USAGE:
   forgex-cli find match <engine>

ENGINES:
   dense         Search with the fully-compiled DFA regex engine.
   lazy-dfa      Search with the lazy DFA regex engine.
   forgex        Search with the top-level API regex engine.
```

</div>

`find`コマンドでは`match`サブコマンドを指定し、その後ろにマッチングに使用する正規表現エンジンを指定します。
エンジンは現在のところ、`lazy-dfa`, `dense`, `forgex`を選択することができます。　　

- `dense`エンジンは、完全にコンパイルされたDFAを使用してマッチングを行います。
- `lazy-dfa`エンジンは、DFAをon-the-flyで構築してマッチングを行います。
- `forgex`を指定すると、Forgexの上位APIを使用してマッチングを行います。これの内部実装は`lazy-dfa`ですが、APIを使用した時間のみが計測されます。

`dense`、`lazy-dfa`、`forgex`の3個いずれかからエンジンを決めたら、通常のFortranコードでForgexのAPIを使って書くのと同様に、`.in.`演算子または`.match.`演算子を使用してパターンと文字列を指定してマッチングを行います。
なお、演算子の右引数を省略した場合には、空文字とのマッチングを試みて結果を表示します。

<div class="none-highlight-user">

```
% forgex-cli find match lazy-dfa "a*b" .match. "ab"
            pattern: a*b
               text: ab
         parse time:        24.6us
   compile nfa time:        39.5us
dfa initialize time:        47.2us
  dfa matching time:       170.5us
    matching result:         T
 memory (estimated):      5707

========== Thompson NFA ===========
state    1: (?, 4)
state    2: <Accepted>
state    3: (b, 2)
state    4: (a, 5)(?, 3)
state    5: (?, 4)
=============== DFA ===============
   1 : a=>2
   2 : b=>3
   3A:
state    1  = ( 1 3 4 )
state    2  = ( 3 4 5 )
state    3A = ( 2 )
===================================
```

</div>

DFAの出力には、上部と下部に分けられます。
上部では、DFA状態番号と、遅延評価により入力文字列から構成されたDFA遷移を記述しています。
下部では、各DFA状態を冪集合構成法で構成されたNFA状態番号のセットを示しています。
ここで、DFA状態番号の後ろに`A`と書かれている場合、そのDFA状態が受理状態であることを意味しています。

なお、このコマンドを実行する際には、いくつかのオプションフラグを指定することができます。

<div class="none-highlight-user">

```
% forgex-cli find match lazy-dfa --help
Executes a search for matches using a lazy DFA regex engine.

USAGE:
   forgex-cli find match lazy-dfa <pattern> .match. <text>
   forgex-cli find match lazy-dfa <pattern> .in. <text>

OPTIONS:
   --verbose     Print more information.
   --no-table    Suppresses the output of the property information table.
   --table-only  Print the property information table only.
```

</div>
