# Wallrs Grammar

**Note:** This is in an early work in progress state and is likely incomplete and inconsistent.

**Note:** Work on the grammar has halted in favor of high-level language design.

```abnf
identifier = /[_a-zA-Z][_a-zA-Z0-9]*/
number = //

value-type = i32 | i64 | f32 | f64

function-type = "fn" "(" value-type* ")" "->" value-type
table-type = "table" "[" number ("," number)? "]"
mem-type = "mem" "[" number ("," number)? "]"
global-type = ("mut" | "const") value-type

import-type = function-type | table-type | mem-type | global-type
export-type = function-type | table-type | mem-type | global-type
any-type = value-type | function-type | table-type | mem-type | global-type

import-item = "from" identifier "import" identifier ":" (import-type | identifier) ";"
export-item = "export" identifier ":" export-type "=" identifier ";"
type-item = "type" identifier "=" (any-type | identifier) ";"

function-item = "fn" "(" function-arg* ")" ("->" value-type)? "{" function-body "}" 
function-arg = identifier ":" type
function-body = TODO

table-item = "table" "[" number ("," number)? "]" table-init?
table-init = "=" "[" table-segments "]"
table-segments = table-segment | table-segments "," table-segment
table-segment = (number ":")? identifier-list
identifier-list = identifier | identifier-list "," identifier

mem-item = "mem" "[" number ("," number)? "]" mem-init?
mem-init = "=" "[" mem-segments "]"
mem-segments = mem-segment | mem-segments "," mem-segment
mem-segment = (number ":")? bytes
bytes = byte | bytes "," byte
byte = /0x[0-9a-fA-F]/

global-item = "global" identifier ":" (global-type | identifier) "=" expr ";"



```

