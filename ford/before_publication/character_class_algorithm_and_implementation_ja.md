---
title: 文字クラスパーサのアルゴリズムと実装
author: 雨崎しのぶ
date: 2025-02-25
---

# 文字クラスパーサのアルゴリズムと実装

## 概要

## 文字クラスについて

正規表現処理系において「文字クラス」とは、

## アルゴリズム

## 実装





## 草稿

- 文字クラス

  - 一文字ずつ走査していく
  - 一つのUnicode文字に注目しているとする
    - エスケープを意味する`\`は前置演算子
    - 範囲を表すハイフン`-`は後置演算子

- 前処理と後処理に分ける

  - 可変長文字列変数とフラグ2つを持つ派生型を定義
    ```fortran
       type :: character_array_t
          character(:), allocatable :: c
          logical :: is_escaped = .false.
          logical :: is_hyphenated = .false.
       end type
    ```

  - 前処理では、文字クラスパターンのバックスラッシュとハイフンをパースして上の派生型の配列に代入する。

  - 後処理では、前処理で作成した配列の文字の値から、セグメントの構築を行う。このとき、前処理で立てた2つのフラグにより条件分岐して、正しいセグメントを構築する。

  ### 前処理：`parse_backslash_and_hyphen_in_char_array`

  - 前処理では入力パターン（文字クラスの部分）を1文字ずつ分解し、派生型の配列の成分`c`に格納する
    - このとき`\`と`-`が出てきたら、格納せずにそれぞれのフラグ`is_escaped`と`is_hyphenated`を立てる。
    - この処理により、後処理において、格納されたパターンの一文字すべてをリテラルとして扱うことができる。

  ### 後処理：`interpret_class_string`

  - メインのループ
    - 注目している文字の`is_hyphenated`の真の場合
      - セグメントの最小値のみに現在の文字のコードポイントを格納する。さらに`prev_hypened`というフラグを立てて、次の文字に移ったときに前の文字がハイフン付きだったかどうかを保持する。最後の`cycle`する
    - `prev_hypened`が真の場合
      - セグメントの最大値のみに、現在の文字のコードポイントを格納する。これは前の文字で最小値のみ格納されたため、ここでセグメントが完成する。
      - ここでセグメントの一時変数をリストに登録する。
      - 最後に`prev_hyphenated`のフラグを降ろして、`cycle`する。
    - `is_backslashed`の真偽で分岐する
      - 真の場合：エスケープ可能な文字の場合は、そのコードポイントをセグメントの最小値および最大値に格納する。対象外の文字の場合は無効なパターンとして`return`する。
      - 偽の場合：現在の文字のコードポイントをそのままセグメントの最大値と最小値に格納する。
      - 最後にリストに登録して次のループに移る
  - ループ後の処理
    - セグメントのリストを引数の配列にコピーして`return`する。