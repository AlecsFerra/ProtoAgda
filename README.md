# Proto agda

A simple implementation of a dependently typed programming language with meta-variables.

## Example

```
def idTy : 𝒰
  = Π (X : 𝒰) -> X -> X

def id : idTy
  = λ _ x. x

def idAgain : (id _ id) _ idTy
  = λ _ x. id _ x

display idAgain
```

Output:
```
idAgain = λ _. λ x. ((id ?26) x) : Π (X : 𝒰) -> Π (_ : X) -> X
Meta context: ?14: Π (X : 𝒰) -> Π (_ : X) -> X,?22: 𝒰,?26: _
```

The engine was able to sove the two metavariables :)
