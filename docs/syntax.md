# Syntax of Flight Rules Logic Code

A flight rule is written using the following grammar, parsed with equal precidence and right accociativity. Adding explicit predicence relations is on the roadmap, but for now add explicit grouping whenever possible.

```
RULE -> EXPRESSION
EXPRESSION -> TERM |  INFIX_OP

TERM -> ATOM | GROUPING | PREFIX_OP
INFIX_OP -> TERM "->" EXPRESSION | TERM "IMPLIES" EXPRESSION
         -> TERM "AND" EXPRESSION
         -> TERM "OR" EXPRESSION
         -> TERM "UNTIL" EXPRESSION
         -> TERM "HOLDS UNTIL WITHIN" float TIME_UNIT EXPRESSION

PREFIX_OP -> "NOT" EXPRESSION
          -> "EVENTUALLY" EXPRESSION
          -> "GLOBALLY" EXPRESSION
          -> "WITHIN" float TIME_UNIT EXPRESSION
          -> "FOR" float TIME_UNIT EXPRESSION
          -> "IF" EXPRESSION "THEN" EXPRESSION

TIME_UNIT -> "MS" | "S" | "H" | "D" | "Y"

GROUPING -> "(" EXPRESSION ")"

ATOM -> NUMERIC_COMPARISON | CATEGORICAL_COMPARISON
     -> STATE_NAME | BOOLEAN_LITERAL

STATE_NAME -> "/([a-zA-Z\d]+(?:_[a-zA-Z\d]+)*)/"

BOOLEAN_LITERAL -> "TRUE" | " FALSE"

CATEGORICAL_COMPARISON -> STATE_NAME "IS" string

NUMERIC_COMPARISON -> STATE_NAME NUMERIC_RELATION float NUM_UNIT
NUMERIC_RELATION -> "<=" | "<" | ">=" | ">" | "=" | "!="
NUM_UNIT -> "AU" | "DEG" | "AMP"

```

## How to write a rule

If the formal specification is too dense follow these steps to understand how to write a flight rule.

1. Remeber that we are writing rules that evaluate to TRUE when conditions are correct, and FALSE when in violations, thus we are specifying the _properties of acceptable behaviour_.
1. Decide on states to model
   - For a boolean state you can simply write the name of the state you want to reason about. State names nust be all alphanumeric and start with a lowercase letter.  
     `a_State_Name_3`
   - For numeric states we must have a valid state name which we can then follow with a numeric comparison operator and a numeric literal. This literal must have an acompanying unit (currently supporting only AU, DEG, and AMP, but we can easily extend this if requested).  
     `some_Numeric_State <= 3 AU`
   - For enumerated states we similarly must write a valid state name however we will use the enumerative comarator `IS` and check the state against an arbitrary string.  
     `enum_state IS "OPERATIONAL-1$#@"`
   - While the syntax supports boolean, numeric, and enumerated states currently we compile them all down to equivalent boolean states (`some_Numeric_State <= 3 AU` becomes a boolean state `some_Numeric_State__<=__3__AU` which is what is queried in the simulation. In future work we will treat these other classes of states as first class and support them within simulations but currently only booleans are supported). Note that these non boolean states must be contiguous so `(a_num_state) > 4 AMP` is invalid.
   - Note that the same state name can be used as many times as you like within a formula to referecne the same state in the given simulation. And any written state that is not found within a simulation is currently assumped to be always FALSE.
1. Once the atomic terms are decided (and ideally acompanying test cases or simulations are loaded) we can build the structure of our rule. The first thing to note is that there is no notion of operator precedence / order of operations, and so be default the rule will be grouped right associativly (`a AND b AND NOT c AND d` is `a AND (b AND ( NOT (c AND d)))`), therefore it is better to be as explicit as possible when writing rules (and always check the diagram to see if the structure of the rule looks correct).

   - Remeber the convention is using lowrcase letters for states, while all operators and rule structural syntax is in ALL CAPS.
   - Recall the definiations of the logical operators explained in the [semantics docs](semantics.md).
   - Terms `a`,`b` can be compared with the `a AND b` and `a OR b` operators, as well as `a -> b` and `a IMPLIES b` as syntax for implication.
   - Alternativly you can write implication as `IF a THEN b`.
   - Terms can be negated with the `NOT a` prefix operator.
   - Unbounded Temporal operators on terms `a` `b` are writen as `GLOBALLY a`, `EVENTUALLY a`, and `a UNTIL b`.
   - The bounded version of eventually is writen as `WITHIN [time] [unit] a`, where `[time]` is a decimal number `3.14159` and unit is a valid time unit, currently only milliseconds `MS`, seconds `S`, hours `H`, days `D`, and years `Y`.  
     `WITHIN 4.2 D a`
   - The bounded version of globally is written as `FOR [time] [units] a` with the same useage of `[time] [units]`.  
     `FOR 37 S a`
   - Finally the bounded version of until is written as `a HOLDS UNTIL WITHIN [time] [units] b`.

So for example we can take this demo rule

> When in mission phase 1 every 2 days the camera is calibrated and the power is cycled

We first decide on the states to model, these are tracking when in mission phase 1 as a boolean state `mission_phase_1`, then we have a boolena state to mark when the camera is being calibrated `camera_calibration` and another for when the power is being cycled `power_cylce`.  
Then we look at the rule description, often a rule is written in the form "when in this case ensure this happens", and similarly this rule follows that structure. We can write that using an implication of

> `IF (mission_phase_1) THEN [TODO]`

Then we see that the thing we need to check is itelf dependent on time "every two days", we can notice that this is equivalant to ensuring that at every point in time, within the next two days something is true. Therefore we can use the `WITHIN 2 D` operator.

> `IF (mission_phase_1) THEN (WITHIN 2 D [TODO])`

Finally we see that what we need to check is that the camera is clibrated _and_ the power is cycled, which naturally we use an `AND` operator to encode

> `IF (mission_phase_1) THEN (WITHIN 2 D (camera_calibratrion AND power_cycle))`

However we note that with how these terms are grouped, and looking at the diagram, this is actually checking that the two things must happen **at the same time** within two days. Instead we want to check that both the camera is calibrated within 2 days, and seperately that the power is cycled within two days. In order to do this we rewrite the rule with more explicit grouping.

> `IF (mission_phase_1) THEN ((WITHIN 2 D (camera_calibratrion)) AND (WITHIN 2 D power_cycle))`

Note how referencing the diagram is generally the best way to ensure that the rule you are writing is being parsed correctly. And it may take some trial and error knowing the rules of what is valid to write to create the diagram/rule you want.

While the vast majority or flight rules can be encoded in this system please let us know if there are any issues you come accross or features that would make writing rules easier or more intuitive.

## Planned Extensions

- Extend Numeric Units
- Extend Grouping characters
- Add heirarchy of precidence for operators
- explicit _type checking_ to make sure units match and enums are well formed etc ...
