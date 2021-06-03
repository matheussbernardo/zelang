# zelang
Zelang is a toy language

# TO DO IN LANG

- Implement top-level scope
In a file 
(define double x (+ x x))
(define quad x (double (double x)))
(quad 10)
- Implement mutability
- Implement side-effects
- Implement scripting language with main entrypoint optional
(define main (quad 4))
- Implement Types
- Implement pattern-matching
- Implement macro system
- Implement struct
- Implement ADT
- Implement ways to create new data structures
- Implement modules
- Convert to compiler?
- Implement VM JIT?

# TO DO IN TOOLING

- Implement a vscode extension as a playground
- Implement language server
- Release binaries easily
- Build system? package system?
