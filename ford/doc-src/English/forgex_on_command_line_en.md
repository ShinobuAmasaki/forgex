---
Title: CLI Tool
Date: 2024-08-15
Author: 雨崎しのぶ
---

# Forgex on Command Line

## Summary

In this project, some test cases to check behavior of the regular expression engine are located in the `test/` directory. We can  promptly run the tests with the command `fpm test`.  In addition, for testing and benchmarking regular expression matches, Forgex provides a command line tool `forgex-cli`, available from version 3.2. For instance, if you want to see whether the pattern `((a|b)*)*` matches the text `ababab`, execute the following command:

<div class="none-highlight-user">

```
% forgex-cli find match lazy-dfa '((a|b)*)*' .match. 'ababab'
```

and you will get the following output on your terminal:

```
            pattern: ((a|b)*)*
               text: 'ababab'
         parse time:        32.6μs
   compile nfa time:        49.5μs
dfa initialize time:        55.7μs
        search time:       643.7μs
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

In this case, the output consists of a table showing the engine's performance, including execution time and memory usage, along with information about the automata (an NFA and a DFA compiled from it) that was built by Forgex. This command can also be run using  `fpm run` as follows:

<div class="none-highlight-user">

```
% fpm run forgex-cli --proflie release -- find match lazy-dfa '((a|b)*)*' .match. 'ababab'
```

</div>

You can use this tool to debug, test, and benchmark regular expression matching. The following sections provide detailed instructions on how to use this command and what its output is.

## Usage

Currently, the commands `debug` and `find` are available.

### `forgex-cli debug`

This command provides information on the process of parsing regular expressions into an abstract syntax tree (AST) and compiling the AST into an NFA. Below is the help message for the `debug` command.

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

If you specify the `ast` subcommand with any regex pattern Forgex accepts, you can get a representation of a nested syntax tree using parentheses.

<div class="none-highlight-user">

```
% forgex-cli debug ast "(a|b)+c?"
        parse time:        38.0μs
memory (estimated):       860
(concatenate (concatenate (or "a" "b") (closure(or "a" "b"))) (or "c" EMPTY))
```

</div>

Alternatively, if you use the `thompson` subcommand, the NFA compiled from the AST is displayed.

<div class="none-highlight-user">

```
% forgex-cli debug thompson "([a-z]*d)+e?"
        parse time:        36.0μs
  compile nfa time:        29.0μs
memory (estimated):     12796
 
========== Thompson NFA ===========
state    1: (?, 6)
state    2: <Accepted>
state    3: (e, 2)(?, 2)
state    4: (?, 8)
state    5: (d, 4)
state    6: (["a"-"c"], 7)(d, 7)(e, 7)(["f"-"z"], 7)(?, 5)
state    7: (?, 6)
state    8: (?, 11)(?, 3)
state    9: (?, 8)
state   10: (d, 9)
state   11: (["a"-"c"], 12)(d, 12)(e, 12)(["f"-"z"], 12)(?, 10)
state   12: (?, 11)

Note: all segments of NFA were disjoined with overlapping portions.
===================================
```

</div>

In the section labeled "Thompson NFA", the NFA is written with one state and its transitions, whether single or multiple, each on a single line. State 2, with `<Accepted>` marked on the right, is special and represents an accepted state of the NFA. Each transition is represented by a character and the destination state number in parentheses, such as `(e, 2)` on state 3. When multiple consecutive characters in the character code table have the same transition destination, they are aggregated and expressed in square brackets like `["a"-"c"]` on states 6 and 11. This is called a segment in the Forgex internal implementation, and is mainly used to improve memory usage efficiency. A transition indicated by  `?` in the character part is a special transition called an ε-transition, which does not consume an input character.

@note As mentioned in the penultimate line of the output, the character segments used for the transitions in the input pattern are split into overlapping parts, e.g. `(["a"-"c"], 7)(d, 7)(e, 7)(["f"-"z"], 7)` in the state 6 . This is called "disjoining" in Forgex development, and is necessary for appropriately assigning transitions to states when constructing a DFA from an NFA using the power set construction method.

### `forgex-cli find`

This command performs matching on the input pattern and string. Below is the help message for the `find` command and the`match` subcommand.

<div class="none-highlight-user">

```
% forgex-cli find --help
Executes a search.

USAGE:
   forgex-cli find <command> ...

COMMANDS:
   match         Search for full matches.
```

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

Specify the `match` subcommand after the `find` command, followed by the regular expression engine to use for matching. Currently, the engine can be selected from `dense`, `lazy-dfa`, or `forgex`.

- The `dense` engine pre-builds and uses a fully compiled DFA from the NFA for matching.
- The `lazy-dfa` engine builds a DFA on-the-fly from the NFA for matching.
- If you specify `forgex`, matching will be performed using the Forgex API module. The internal implementation is lazy DFA, but only the overall time spent using the API is measured.

Once you have selected one of the three engines, you can execute the command by specifying a pattern and string using the `.in.` or `.match.` operator, just like you would write normal Fortran code using the Forgex API. If you omit the right operand, it will output the result of matching against an empty string.

<div class="none-highlight-user">

```
% forgex-cli find match dense '(a|b)*c?' .match. 'ababac'
            pattern: (a|b)*c?
               text: 'ababac'
         parse time:        24.0μs
   compile nfa time:        19.0μs
dfa initialize time:         9.0μs
   compile dfa time:        37.0μs
        search time:        56.0μs
    matching result:         T
 memory (estimated):      5812
 
========== Thompson NFA ===========
state    1: (?, 4)
state    2: <Accepted>
state    3: (c, 2)(?, 2)
state    4: (["a"-"b"], 5)(?, 3)
state    5: (?, 4)
=============== DFA ===============
   1A: c=>2 ["a"-"b"]=>2
   2A: c=>2 ["a"-"b"]=>2
state    1A = ( 1 2 3 4 )
state    2A = ( 2 3 4 5 )
===================================
```

</div>

The NFA display is the same as that of the `forgex-cli debug` command. The output of the DFA is divided into upper and lower parts. The upper part lists the DFA state numbers and DFA transitions. The lower part shows a set of NFA states constructed for each DFA state using the power set construction method. Here if `A` is written after the DFA state number, it means that the DFA state is an accepting state.

### Performance Information Table

Forgex performance table, including execution time, memory usage, and results, is shown with every `forgex-cli` command example in the previous section. In this section, we'll explain what each table entry means, but first a quick rundown of the option flags available with forgex-cli. When you run the `forgex-cli` command, you can specify several option flags. For example, running `find match lazy-dfa --help` will display the following help message:

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

Each item listed in the `OPTIONS` section mean:

- `--verbose`: This option provides more detailed information in the properties table, offering deeper insights into matching process.
- `--no-table`: This option suppresses the properties table, allowing the output to focus exclusively on the structure of the NFA and DFA automata generated during the matching process.
- `--table-only`: This option limits the output to just properties table, omitting details about the NFA nad DFA, which may be useful when you need a quick overview of performance metrics.

Here we will look at an example using the `--table-only` option flag to output only property information. First, below is a example of the command `forgex-cli find match lazy-dfa`:

<div class="none-highlight-user">

```
% forgex-cli find match lazy-dfa "([a-z]*g)+n?" .match. "assign"
            pattern: ([a-z]*g)+n?
               text: 'assign'
         parse time:        29.0μs
   compile nfa time:        28.0μs
dfa initialize time:         3.0μs
        search time:       144.0μs
    matching result:         T
 memory (estimated):     13736
```

</div>

`pattern` and `text` show the pattern and string that were specified when the command was executed. Below that, the measured times are shown: `parse time` shows the time to build an AST from the specified regular expression,  `compile nfa time` shows the time to compile an NFA from it, `dfa initialize time` shows the time to initialize the DFA to prepare before characters are entered, and `search time` is the time it takes for the DFA engine to execute after receiving an input character, i.e., the time it takes to make a match. The `lazy-dfa` engine waits for character input and builds the DFA, so `initialize time` and `search time` are measured. `matching result` is a logical value indicating the result of the actual matching. `memory (estimated)` shows the static size of memory in bytes calculated from memory allocation information of AST, NFA, and DFA objects at the end of matching execution.

On the other hand, `dense` engine outputs a table which is different to above. For example:

<div class="none-highlight-user">

```
% forgex-cli find match dense "([a-z]*g)+n?" .match. "assign" --table-only
Project is up to date
            pattern: ([a-z]*g)+n?
               text: 'assign'
         parse time:        16.0μs
   compile nfa time:        29.0μs
dfa initialize time:         4.0μs
   compile dfa time:        35.0μs
        search time:        47.0μs
    matching result:         T
 memory (estimated):     15480
```

</div>

 `compile dfa time`  is measured on the `dense` engine. Note that the memory usage of the `dense` engine is equal to or more than that of the `lazy-dfa` engine.

What will be displayed if you specify `forgex` as the engine of the command?

<div class="none-highlight-user">

```
% forgex-cli find match forgex "([a-z]*g)+n?" .match. "assign"
pattern: ([a-z]*g)+n?
   text: "assign"
   time:       229.0μs
 result:         T
```

</div>

In this case, the only performance information provided is the `time` measured before and after the API call. This is because in Forgex version 3 and later, all procedures that compose the API (`.in.` and `.match.` operators) have the `pure` attribute, which means that operations with side effects, such as internal time measurement, cannot be performed.

If you use the `--verbose` flag with any engine other than `forgex`, you can get detailed information about how many AST, NFA and DFA objects were used.

<div class="none-highlight-user">


```
% forgex-cli find match lazy-dfa "([a-z]*g)+n?" .match. "assign" --verbose --table-only
            pattern: ([a-z]*g)+n?
               text: "assign"
         parse time:        21.0μs
   compile nfa time:        32.0μs
dfa initialize time:         3.0μs
  dfa matching time:       149.0μs
    matching result:         T
 memory (estimated):     13736
    tree node count:        10/32
         nfa states:        12/16
         dfa states:         5/16
```

</div>

For each of `tree node count`, `nfa states`, and `dfa states`, the denominator represents the allocated memory, while the numerator shows the amount actually used. 

@note The counts of `tree node count` and `nfa states` are the same for the `dense` engine and the `lazy-dfa` engine, but the count of `dfa states` may be larger for `lazy-dfa` than for `dense`.

## Conclusion

The `forgec-cli` tool provides a command line interface for testing, debugging, and benchmarking regular expression engines. With features that support engines like `dense`, `lazy-dfa`, and `forgex`, users can analyze regex matching in different contexts and performance scenarios.

### Key Points:

#### 1. Engine Options and Performance Insights:

- The `dense` engine utilizes a fully compiled DFA for fast matching, but it may cosume more memory. Additionally, for certain complex regular expressions, the DFA construction can be quite time-comsuming, which might affect overall performance.
- The `lazy-dfa` engine constructs the DFA on-the-fly, offering a more memory-efficient approach at the cost of potentially longer search times.
- The `forgex` engine provides a top-level API for regex operations, but its performance metrics are limited to overall execution time due to its `pure` attribute.

#### 2. Command Usage:

- `forgex-cli debug` helps visualize the parsing and compilation process with `ast` and `thompson` subcommands.
- `forgex-cli find` performs regex matching and provides detailed performance and memory usage statistics. The `--verbose` flag offers additional information about the matching process, while the `--table-only` flag allows you to focus specifically on performance metrics by filtering out other details.

#### 3. Performance Metrics:

- Users can access detailed breakdonws of execution times, memory usage, and internal state counts for different engines.
- For the `lazy-dfa` engine, additional into NFA nad DFA object usage can be obtained, highlighting the efficency and trade-offs of the engine's on-the-fly DFA construction.

Overall, `forgex-cli` aims to be a versatile tool for evaluating regular expression performance, providing engine choices and detailed diagnostics that help understand the regular expression matching process. However, it is important to note that for certain types of regular expressions, especially complex ones, building a DFA in a `dense` engine can be very time and memory consuming. This is why the internal implementation of the Forgex API uses Lazy DFA.

## Acknowledgements

The command line interface design for this application was inspired by [the Rust language's `regex-cli`](https://github.com/rust-lang/regex/tree/master/regex-cli).













