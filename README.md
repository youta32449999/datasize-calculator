# datasize-calculator

## データ型のBNF
<data-type> ::= "TYPE" {<structure>} "END_TYPE"
<structure> ::= <array> | <struct>
<data-type-name> ::= "[0-9A-z]+"
<array> ::= <data-type-name> ":" "ARRAY[" <digits> ".." <digits> "]" "OF" <data-type-name> ";"
<struct> ::= <data-type-name> ":" "STRUCT" {<member-name> ":" <data-type-name> ";"} "END_STRUCT" ";"
<digits> ::= "[0-9]+"
<member-name> ::= "[0-9A-z]+"
