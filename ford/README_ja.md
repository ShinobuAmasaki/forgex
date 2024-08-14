Forgexは、すべてFortranで書かれた正規表現エンジンです。

このプロジェクトは[Fortranパッケージマネージャー](https://fpm.fortran-lang.org/ja/index.html)で管理され、
正規表現の基本的な処理を提供し、[MITライセンス](https://github.com/ShinobuAmasaki/forgex/blob/main/LICENSE)
のもとで利用可能なフリーソフトウェアです。
エンジンの核となるアルゴリズムには決定性有限オートマトン（Deterministic Finite Automaton, DFA）を使用しています。
この選択は実行時パフォーマンスを重視したものです。

## 機能

Forgexが処理を受け付ける正規表現の記法は以下の通りです。

### メタキャラクター
- `|`  選言（alternation）のバーティカルバー
- `*`  ゼロ回以上にマッチするアスタリスク
- `+`  一回以上にマッチするプラス記号
- `?`  ゼロ回または一回にマッチするクエスチョンマーク
- `\`  メタキャラクターのエスケープ
- `.`  任意の一文字にマッチするピリオド

### 文字クラス
- 文字クラス（例： `[a-z]`）
- 否定クラス（例: `[^a-z]`）
- Unicode文字クラス（例: `[α-ωぁ-ん]`）

否定クラスは制御文字にはマッチしないことに注意してください。

### 繰り返し回数の指定
- `{num}`,
- `{,max}`,
- `{min,}`,
- `{min, max}`,
ここで `num`と`max`は0（ゼロ）以外の自然数を指定します。

### アンカー
- `^`, 行頭にマッチ
- `$`, 行末にマッチ

### 略記法
- `\t`, タブ文字
- `\n`, 改行文字 (LFまたはCRLF)
- `\r`, 復帰文字 (CR)
- `\s`, 空白文字 (半角スペース, タブ文字, CR, LF, FF, 全角スペース U+3000)
- `\S`, 非空白文字
- `\w`, ラテン文字アルファベット、半角数字及びアンダースコア(`[a-zA-Z0-9_]`)
- `\W`, `\w`の否定クラス(`[^a-zA-Z0-9_]`)
- `\d`, 半角数字 (`[0-9]`)
- `\D`, 非半角数字 (`[^0-9]`)

## 使用方法
動作確認は以下のコンパイラーで行っています。

- GNU Fortran (`gfortran`) v13.2.1
- Intel Fortran Compiler (`ifx`) 2024.0.0 20231017

以下では、ビルドとAPIの使い方について解説しますが、Fortranパッケージマネージャー（`fpm`）を利用することを前提とします。

### ビルド
まず初めに、あなたのプロジェクトの`fpm.toml`に以下の記述を追加します。

```toml
[dependencies]
forgex = {git = "https://github.com/shinobuamasaki/forgex", tag="v2.0"}
```


### APIの使い方
そのプロジェクトのプログラムのヘッダーに`use forgex`と記述すると、`.in.`と`.match.`の演算子、
`regex`サブルーチンと`regex_f`関数が導入され、`use`文の有効なスコープでこれらの4つを使用することができます。

```fortran
program main
   use :: forgex
   implicit none
```

`.in.`演算子は、文字列型を引数にとり、第一引数のパターンが、第二引数の文字列に含まれる場合に真を返します。

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

`.match.`演算子は、同様に指定されたパターンが、厳密に文字列と一致する場合に真を返します。

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

`regex`関数は、入力文字列の中でパターンに一致した部分文字列を返します。
```
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

オプショナル引数の`from`/`to`を使用すると、与えた文字列から添字を指定して部分文字列を切り出すことができます。
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

   ! Cut out the matched part that equivalent to the result of the `regex` function.
   print *, str(from:to)         ! def

   ! Cut out after the matched part.
   print *, str(to+1:len(str))   ! ghi

end block
```

`regex`関数の宣言部（インタフェース）は次の通りです。
```fortran
interface regex
   module procedure :: subroutine__regex
end interface

pure subroutine subroutine__regex(pattern, text, res, length, from, to)
   implicit none
   character(*),              intent(in)    :: pattern, text
   character(:), allocatable, intent(inout) :: res
   integer,      optional,    intent(inout) :: length, from, to
```

マッチした文字列を関数の戻り値として得たい場合には、`regex_f`関数を使用してください。

```fortran
interface regex_f
   module procedure :: function__regex
end interface regex_f

pure function function__regex(pattern, text) result(res)
   implicit none
   character(*), intent(in)  :: pattern, text
   character(:), allocatable :: res
```

### UTF-8文字列のマッチング

UTF-8の文字列についても、ASCII文字と同様に正規表現のパターンで一致させることができます。
以下の例は、漢文の一節に対してマッチングを試みています。

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

この例では`length`変数にバイト長が格納され、この場合は10個の3バイト文字に一致したので、その長さは30となります。

### 注意

- WindowおよびmacOS環境の`gfortran`でコンパイルされたプログラムでは、OpenMPの並列ブロックの中で割り付け可能文字列型変数を使用すると、セグメンテーション違反などでプログラムが停止する可能性があります。
- コマンドラインツール`forgex-cli`をWindows上のPowerShellで利用する場合、システムのロケールをUTF-8に変更する必要があります。

## To Do
- Unicodeエスケープシーケンス`\p{...}`の追加
- UTF-8において無効なバイトストリームへの対処
- リテラル検索によるマッチングの最適化
- ✅️ デバッグおよびベンチマーク用のCLIツールを追加
- ✅️ すべてのAPI演算子に`pure elemental`属性を追加
- ✅️ ドキュメントの公開
- ✅️ UTF-8文字の基本的なサポート
- ✅️ On-the-FlyのDFA構築
- ✅️ CMakeによるビルドのサポート
- ✅️ 簡単な時間計測ツールの追加
- <s>マッチングの並列化</s>

## コーディング規約
本プロジェクトに含まれるすべてのコードは、3スペースのインデントで記述されます。

## 謝辞
冪集合構成法のアルゴリズムと構文解析については、Russ Cox氏の論文と近藤嘉雪氏の本を参考にしました。
優先度付きキューの実装は、[ue1221さんのコード](https://github.com/ue1221/fortran-utilities)に基づいています。
文字列に対して`.in.`演算子を適用するというアイデアは、soybeanさんのものにインスパイアされました。
`forgex-cli`のコマンドラインインターフェイスの設計については、Rust言語の`regex-cli`を参考にしました。
## 参考文献
1. Russ Cox ["Regular Expression Matching Can Be Simple And Fast"](https://swtch.com/~rsc/regexp/regexp1.html), 2007年
2. 近藤嘉雪, "定本 Cプログラマのためのアルゴリズムとデータ構造", 1998年, SB Creative.
3. [ue1221/fortran-utilities](https://github.com/ue1221/fortran-utilities)
4. [kazulagi, @soybean](https://github.com/kazulagi), [Fortranでユーザー定義演算子.in.を作る - Qiita.com](https://qiita.com/soybean/items/7cdd2156a9d8843c0d91), 2022年
5. [rust-lang/regex/regex-cli](https://github.com/rust-lang/regex/tree/master/regex-cli)

## ライセンス
このプロジェクトはMITライセンスで提供されるフリーソフトウェアです
（cf. [LICENSE](https://github.com/ShinobuAmasaki/forgex/blob/main/LICENSE)）。
