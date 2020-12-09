# Semantics of Flight Rules Logic

This document is mean to be the normaltive specifcation of the flight rules logical sysetm and such any differences between the implmentation found in the tool and this document should be reported as implemntation bugs. This document should be used as the source for building integrations with other simulation or verification platforms (however note code blocks in this document are meant to illustrate logical structure and not actual implemntation code). For further reference, this specification is based on [Signal Temporal Logic](https://dl.acm.org/doi/pdf/10.1145/3290364).

## States

Flight rules specify **the required behaviour of states** for a mission. States represent primitive values of the current state of the mission. At every single point in time, every state has a value. States can thus be conceptualized as functions from the time domain of the mission to thier specific type. Therefore the flight rule being build of as a composite of these states represents also a function from time to {TRUE,FALSE}, where TRUE represents a time where the rule is being validated, and FALSE when it is violated. This is to say rules are composed of **Boolean Terms**

```elm
BooleanTerm : Time → {TRUE, FALSE}
```

Which can be either composed using logical operators, or constructed from atomic states. States then can be broken down into three essential categories

1. Boolean States
2. Numeric States
3. Enumerated States

### Boolean States

```elm
BooleanState : Time → {TRUE, FALSE}

```

Boolean states are the most simple representation of states. They represent values that have either a TRUE or FALSE value at every given point in time. An example would be if we wanted to keep track of a physical property that is naturally true/false such as `lights_on` which would evaluate to true at any point where the lights are on, and false when the lights are off. These states can be used 'as is' inside of a flight rule formula as a `BooleanTerm`.

### Numeric States

```Elm
NumericState : Time → ℝ

ComparisonOp : NumericState → NumericLiteral → BooleanTerm
=   >
|   <
|   <=
|   >=
|   ==
|   !=

NumericLiteral : (ℝ , Unit)
= (ℝ , V)   --Volts
| (ℝ , AMP) --Amperes
| (ℝ , DEG) --Degrees
| (ℝ , AU)  --Astronomical Units

```

Numeric states are states which evalaute to a real number at any given point in time. This is the most common state for encoding physical values such as `voltage` or `velocity`. Numeric states must be specified with units. In order to use numeric states within a flight rule the real number must be converted to a boolean through a comparison operator: `> >= < <= == !=`. Numeric states must be compared to literal numeric values which then produces a boolean term that can be used in the flight rule. Currently numeric states must be compared to contant literal values and cannot be compared to other numeric states. For example `(voltage > 12 V)` is a valid term, while `(voltage)` , `(volatage > 12)`, `(voltage > velocity)`, and `(voltage_1 > volatge_2)` are not.

### Enumerated States

```elm
EnumState : Time → EnumValues

is : EnumState → String → BooleanTerm

```

Enumerated States are states whose codomain are neither boolean nor numeric, and thus must be explictly enumerated in the definition of the state. For example a device with three operational modes `A`, `B`, and `C` and a possiblity of being nonoperational, could be defined as an enumerated state with possible values `{"A", "B", "C", "NONOP"}` so that at every given point in time the value of the state is one of these values (which we encode explicitly as strings). We can then use an enumerated state in a formula by checking which of these enumerated values the state is using the keyword `is`. For example `(device is "A")`.

## Boolean Operators

The above state declarations form the atoms of flight rules. These terms can then be combined using logical operators to form higher order terms. Boolean operators are the logical operators that are atemporal, the boolean value of a term at a given time only depends on the boolean values of subterms at that same time. The boolean operators supported in flight rule logic are

1. Conjunction (`AND`, ∧)
2. Disjunction (`OR`, ∨)
3. Negation (`NOT`, ¬)
4. Implication (`IMPLIES`, `IF`/`THEN`, →)

### Conjunction

Two terms (`α β : BooleanTerm`) can be combined to form another `BooleanTerm` using the conjunction opperator `AND`. The resulting term `α AND β` is a term which evaluates at each time point, and takes the logical conjunction of the evaluation of each of `α` and `β` at that same time point.

```elm
AND : (α : BooleanTerm) → (β : BooleanTerm) → BooleanTerm
= (λ t : Time,  α(t) ∧ β(t))
```

For reference the operator `∧` has truth table

| a ∈ {TRUE, FALSE} | b ∈ {TRUE, FALSE} | (a∧b) |
| ----------------- | ----------------- | ----- |
| TRUE              | TRUE              | TRUE  |
| TRUE              | FALSE             | TRUE  |
| FALSE             | TRUE              | TRUE  |
| FALSE             | FALSE             | FALSE |

### Disjunction

Two terms (`α β : BooleanTerm`) can be combined to form another `BooleanTerm` using the disjunction opperator `OR`. The resulting term `α OR β` is a term which evaluates at each time point, and takes the logical disjunction of the evaluation of each of `α` and `β` at that same time point.

```elm
OR : (α : BooleanTerm) → (β : BooleanTerm) → BooleanTerm
= (λ t : Time,  α(t) ∨ β(t))
```

For reference the operator `∨` has truth table

| a ∈ {TRUE, FALSE} | b ∈ {TRUE, FALSE} | (a∧b) |
| ----------------- | ----------------- | ----- |
| TRUE              | TRUE              | TRUE  |
| TRUE              | FALSE             | FALSE |
| FALSE             | TRUE              | FALSE |
| FALSE             | FALSE             | FALSE |

### Negation

A term (`α : BooleanTerm`) can be negated to form another `BooleanTerm` using the negatio n opperator `NOT`. The resulting term `NOT α` is a term which evaluates at each time point, and negates the evaluation of `α`.

```elm
NOT : (α : BooleanTerm)  → BooleanTerm
= (λ t : Time,  ¬α(t))
```

For reference the operator `¬` has truth table

| a ∈ {TRUE, FALSE} | (¬a)  |
| ----------------- | ----- |
| TRUE              | FALSE |
| FALSE             | TRUE  |

### Implication

Two terms (`α β : BooleanTerm`) can be combined to form another `BooleanTerm` using the implication operator `IMPLIES`. The resulting term `α IMPLIES β` (which can also be written `IF α THEN β`) is a term which evaluates at each time point, and takes the logical material conditional of the evaluation of each of `α` and `β` at that same time point.

```elm
IMPLIES : (α : BooleanTerm) → (β : BooleanTerm) → BooleanTerm
= (λ t : Time,  (¬α(t) ∨ β(t)))
```

For reference the operator has truth table

| a ∈ {TRUE, FALSE} | b ∈ {TRUE, FALSE} | (a⇒b) |
| ----------------- | ----------------- | ----- |
| TRUE              | TRUE              | TRUE  |
| TRUE              | FALSE             | FALSE |
| FALSE             | TRUE              | TRUE  |
| FALSE             | FALSE             | TRUE  |

## Temporal Operators

In addition to standard propositional logical operators flight rules may need to reason about time, this is done using temporal operators. This set of temporal operators is based on [Signal Temporal Logic](https://dl.acm.org/doi/pdf/10.1145/3290364) as well as [Linear Temporal Logic](https://en.wikipedia.org/wiki/Linear_temporal_logic). Like boolean operators these temporal operators will tranform input Boolean Terms and produce Boolean Terms. However the value of these operators at any given time depends on the whole time series of each input. The inuition for these operators is that the value of the resulting Boolean Term at a given time is the value of the temporal operator acting "from the persepctive" of that given instant and then "looking forward/backward" in time on the input terms to produce the specific truth value for each timepoint. This will be fully formalized in the definitions provided. The core set of temporal operators for flight rules are :

1. `EVENTUALLY` (sometimes in literature `◊`)
2. `GLOBALLY` (sometimes in literature `□`)
3. `UNTIL`

And each of these operators contain explicit time bounded versions as well.

### EVENTUALLY

Given Boolean Term `α`, we can produce the term `EVENTUALLY α`. The resulting term evaluates to true at each time point `t` where for at least one `t₀` in the future, we know that `α(t₀)` is true.

```elm
EVENTUALLY : (α : BooleanTerm) → BooleanTerm
= (λ t : Time,  (∃(t₀:Time)>=t, α(t_prime) ))
```

### GLOBALLY

Given Boolean Term `α`, we can produce the term `GLOBALLY α`. The resulting term evaluates to true at each time point `t` where for all points `t₀` in the future, we know that `α(t₀)` is true.

```elm
GLOBALLY : (α : BooleanTerm) → BooleanTerm
= (λ t : Time,  (∀(t₀:Time)>=t, α(t_prime) ))
```

### UNTIL

Given `α β : BooleanTerm`, we can produce the term `α UNTIL β`. The resulting term evaluates to true at each time point `t` where there exists a point `t₀` in the future where we know that `β(t₀)` is true, and that for all times `t₁` where `t<=t₁<t₀` we know that `α(t₁)` is true. So thus `α` holds **until** `β`. Note that in fact both `GLOBALLY` and `EVENTUALLY` are special cases of `UNTIL` but are more common and less complicated and so are a useful abstraction.

```elm
UNTIL : (α : BooleanTerm) → (β : BooleanTerm) → BooleanTerm
= (λ t : Time,  (∃(t₀:Time)>=t, (β(t₀) ∧ (∀(t₁:Time)∈[t,t₀), α(t₁))  ))
```

### Bounded Operators

Each of these operators have time bounded versions, this is the addition that extends Linear Temporal Logic into Signal Temporal Logic. For the previous operators note how the choice of future time `t₀` was for any time greater than `t`, implicitly going on to infinity / the length of the simulation. We can create bounded versions by adding a limit to how far ahead we can look by `r`. Similarly it was implicit how the starting time for looking was simply `t`, however we can offset this start time by `s`. This produces the bounded versions of the operators.

```elm
BoundedEventually : (α : BooleanTerm) → (s : TimeDuration) → (r : TimeDuration) → BooleanTerm
= (λ t : Time,
      (∃(t₀:Time)∈[t+s,t+r) α(t_prime) ))

BoundedGlobally : (α : BooleanTerm) → (s : TimeDuration) → (r : TimeDuration) → BooleanTerm
= (λ t : Time,
      (∀(t₀:Time)∈[t+s,t+r) α(t_prime) ))

BoundedUntil : (α : BooleanTerm) → (s : TimeDuration) → (r : TimeDuration) → BooleanTerm
= (λ t : Time,
      (∃(t₀:Time)∈[t+s,t+r),
        (β(t₀) ∧ (∀(t₁:Time)∈[t+s,t₀), α(t₁))  ))
```

Finally note that in fact we can choose negative values for `r` and `s`. This models "looking backwards in time".

## Planned Extensions

- automatic ε error extentionality

  - it might be nice to have built in checking against data that is close to violation either in time or in real number value when relevant. We could encode this be adding in a user editable risk tolerance parameter.

- mutual numeric state comparator

  - it would be nice to be able to compare numeric states (with same units or same quanity) against each other directly

- cumulative operator
  - there are some rules that involve some accumulation, (i.e. we cant be doing this that and another thing all at once for more than a total of n hours). Currently we would be forced to encode the cumuklative time under a condition as a standalone numeric state. This however would outsource the logical complexity of the condition which would likely be better expressed within the rule itself. Therefore it would be nice to have a cumulation operator that can caluclate the (potentially bounded) total time true for a given term (which we could then treat as a standalone new 'numeric state').
