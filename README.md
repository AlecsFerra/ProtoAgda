# Proto agda

A simple implementation of a dependently typed programming language with meta-variables.

## Example

```
def idTy : 𝒰
  = Π (X : 𝒰) -> X -> X

def id : idTy
  = λ _ x. x

def idAgain : id _ idTy
  = λ _ x. id _ x

display idAgain
```

Output:
```
idAgain = λ _. λ x. ((id ?17) x) : Π (X : 𝒰) -> Π (_ : X) -> X
Meta context: ?13: 𝒰,?17: _
```

The engine was able to sove the two metavariables :)
