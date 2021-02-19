# harakiri lib

Library for parsing, type checking and compiling a simple programming language named harakiri. It's implemented without LLVM or GCC backend. 

## Supported architectures

* Aarch64

## Syntax

All supported constructions are used in the code below:

```
def fib(n) {
    if n <= 2 {
        return 1
    }
    return fib(n-1) + fib(n-2)
}

def print_interval(from, to) {
    while from < to {
        echo(from)
        from = from + 1
    }
}

def main() {
    echo("Enter mode: 0 - interval printer, 1 - fibonacci calculator")
    a = -1
    while 1 {
        a = input()
        if a != 0 && a != 1 {
            echo("Mode must be equal to 0 or 1")
        } else {
            break
        }
    }
    
    if a == 0 {
        echo("enter start value")
        a = input()
        echo("enter end value")
        b = input()
        print_interval(a, b)
    } else {
        echo("enter n to get n-th fibonacci number")
        a = input()
        echo(a, "-th fibonacci number is: ", fib(a))
    }
}
```

## Supported operations

| operation | unary or binary | description              |
|-----------|-----------------|--------------------------|
| -         | unary           | negate                   |
| +         | binary          | addition                 |
| -         | binary          | subtraction              |
| *         | binary          | multiplication           |
| /         | binary          | division                 |
| ==        | binary          | equal to                 |
| !=        | binary          | not equal to             |
| <         | binary          | lower than               |
| <=        | binary          | lower than or equal to   |
| >         | binary          | greater than             |
| >=        | binary          | greater than or equal to |
| &&        | binary          | boolean and              |
| \|\|      | binary          | boolean or               |

## Implemented parts

- [x] Parser
- [x] Type checker
- [x] IR generation
- [x] Assembly language code generation
- [ ] IR optimizations
