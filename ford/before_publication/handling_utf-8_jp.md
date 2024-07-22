# UTF-8文字の取り扱い

## 概要

## 詳細

### UTF-8による符号化のアルゴリズム

### Fortranにおける取り扱い

### Forgexの実装

#### エンコード

Forgexにおいて、UnicodeコードポイントからUTF-8文字（32ビットバイナリ列）への変換は、モジュール`forgex_utf8_m`に定義されている関数`char_utf8`が担う。

#### デコード

Forgexにおいて、UTF-8文字（32ビットバイナリ列）からUnicodeコードポイントへの変換は、モジュール`forgex_uftf8_m`に定義されている関数`ichar_utf8`が担う。

### 不正なバイトシーケンスの処理

(work in progress...)

## 参考文献

1. [Using Unicode Characters in Fortran - Fortran-lang Discourse](https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764)