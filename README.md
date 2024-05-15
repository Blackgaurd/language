# language

*My own little scripting language. I tried coming up with a more creative name but unfortunately I specialize in writing code not fiction.*

*language* is an interpreted, imperative, dynamically typed and garbage collected scripting langauge. The current implementation is in Haskell with cabal. It supports feature's one would expect in a scripting language, such as variables, conditional branching, loops and functions. On top of this it also has a few unique features like infix operator declarations.

Here's a snippet:

```
infixl divides(a, b)[5] {
    return b % a == 0;
}
proc is_prime?(x) {
    %% this is a comment
    %% 1 is not prime
    when x == 1 | 2 `divides` x then return !f;
    i = 3;
    while i * i <= x then {
        when i `divides` x then return !f;
        i = i + 2;
    }
    return !t;
}
proc main() {
    i = 1;
    while i <= 100 then {
        !disp(i);
        when is_prime?(i) then {
            !displn(" is prime");
        } otherwise {
            !displn(" is not prime");
        }
        i = i + 1;
    }
}
```

## building from source

To build *language*, you will need [cabal, installed through ghcup](https://www.haskell.org/ghcup/). To build:

```
git clone https://github.com/Blackgaurd/language.git
cabal install language --installdir=.
```

To run a language file, called `program.lang`:

```
./language program.lang
```

## language syntax

The syntax of language is similar to JavaScript. Blocks are surrounded by curly braces, and statements end with a semicolon. For formal langauge grammar, refer to [the BNF grammar](./language.grammar).

Below is a language program that covers most core features to get started:

```
%% a language program is made up of procedures
%% execution begins at the main procedure
proc main() {

    %% variables are dynamically typed
    x = 1;
    x = !t; %% true
    x = "my string";

    %% identifiers are case sensitive, and can include the ! and ? characters
    wow! = 17;
    wow? = 17;
    !?!?!? = -1;

    %% integers support all standard operators
    %% they are unbounded
    y = -x * x + x - x / x % x;
    y = (1 + 2) * 3;

    %% integers are totall ordered
    b = 1 == 1;
    b = 1 ~= 1; %% not equal
    b = 1 <= 1; %% etc.

    %% booleans are declared using !t for true and !f for false
    %% there is the 'or' logical operator |
    %% and the 'and' logical operator &
    %% logical 'not' is with the ~ operator
    myTrue = !t;
    myFalse = !f & 1 / 0 == 0; %% & and | use short-circuiting
    myFalse = ~!t;

    %% strings are immutable, but support concatenation and indexing
    hello = "hello";
    world = ", world!\n";
    greeting = hello + world;
    comma = greeting @ 5; %% comma = ","

    %% output a variable using !disp or !displn
    !disp(123);
    !displn(4567);

    %% procedures are called as follows
    six = average(6, 7, 5);

    %% conditional execution is done with the 'when ... then ... otherwise' keywords
    when 1 == 1 then {
        %% do something
    }
    when 1 ~= 1 then !displn("yes!"); %% braces are not necessary for one statement
    otherwise {
        !displn("no!");
    }

    %% loops are performed with the 'while ... then' keywords
    i = 0;
    while i < 3 then {
        !disp(i);
        i = i + 1;
    }
}
proc average(a, b, c) {
    sum = a + b + c;
    return sum / 3;
}
```

Below details language in more detail.

### comments

Only single-line comments are supported. They begin with `%%`.

### program structure

A language program is composed of *procedures* which can take arguments. The program starts execution from the `main` procedure, as follows:

```
proc proc1(x) {
    return x + 1;
}
proc main() {
    a = proc1(4);
}
```

### variables

Variables are declared `identifier = expression;`. If a variable does not previously exist, it will be created. Otherwise, the existing binding will have its value modified. For example,

```
x = 14;
y = 3 + 5 / 2; %% y = 5
```

Identifiers are case sensitive, and can be composed of alpha numeric characters alongside `_`, `!` and `?`. Identifiers need not start with an alpha character. (*Note: this is inspired by identifier rules in Racket.*)

```
%% valid identifiers:
hello_ = 0;
HELLO! = 0;
2Hs = "hh";
number? = 0;

%% invalid identifiers:
444 = 0;
```

Integers are unbounded, support closed arithmetic operators `+`, `-`, `*`, `/`, `%` defined by standard precedence. Comparissons are done with `==`, `~=` (not equal), `<`, `>`, `<=`, `>=` which return boolean values.

The boolean literals are `!t` for true, and `!f` for false. They support the closed logical operators `~` (not), `|` (or), `&` (and), which perform short-circuiting.

Strings are declared with double quotes: `"my string 123\n"`, and are immutable. They can be indexed using the `@` operator: `"abc" @ 1`. Strings can be concatennated with the `+` operator.

### blocks

Blocks of code with local scope can be used with curly braces. For example:

```
x = 1;
{
    x = 2;
    a = 1;
    !displn(a); %% 1
}
!displn(x); %% 2
!displn(a); %% error: a is not defined
```

### conditionals

Conditional execution is performed using the `when <expression> then <statement>` and `when <expression> then <statement> otherwise <statement>` blocks. For example:

```
i = 5;
when i < 100 then {
    !displn("under 100");
} otherwise {
    !displn("over 100");
}
```

### loops

Loops are performced using the `while <expression> then <statement>` block. For example:

```
i = 0;
j = 10;
while i <= j then {
    i = i + 1;
    j = j - 1;
}
```

### builtins

All builtin functions are prefixed by a `!` character.

| builtin | functionality |
| :---: | :--- |
| `!disp(x, y, ...)` | outputs `x`, `y` and all other arguments to `stdin`, separated by spaces |
| `!displn(x, y, ...)` | outputs `x`, `y` and all other arguments to `stdin`, separated by spaces and ending with a newline |
| `!type(x)` | returns a string `"integer"`, `"boolean"` or `"string"` based off the type of `x` |
| `!ascii(x)` | takes a string of length 1, and returns its ascii value |

### infix operator definitions

Infix operators can defined similarly to procedures, but they must take two arguments: the left and right hand sides of the operator. They are called by surrounding the operator name with backticks. Precedence for the operator must be specified (higher number means evaluated sooner), and associativity can be made left or right with the `infixl` or `infixr` keywords respectively. For example:

```
%% right associative operator with precedence 4 (same as addition)
infixr dispThenAdd(a, b)[4] {
    !disp("adding ");
    !disp(a);
    !disp("+");
    !disp(b);
    !displn();
    return a + b;
}
int main() {
    1 `dispThenAdd` 2 `dispThenAdd` 3;
}
```

The above would output:

```
adding 2+3
adding 1+5
```

Parentheses have highest precedence. Unary operators have 100 precedence. The builtin operators are all left associative, and they precedence values are as follows:

| operator | precedence |
| :---: | :---: |
| `&` | 1 |
| `|` | 1 |
| `==` | 2 |
| `~=` | 2 |
| `<` | 3 |
| `<=` | 3 |
| `>` | 3 |
| `>=` | 3 |
| `+` | 4 |
| `-` | 4 |
| `*` | 5 |
| `/` | 5 |
| `%` | 5 |
| `@` | 6 |
