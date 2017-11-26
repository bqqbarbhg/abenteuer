Abenteuer
=========

Text adventure for Aalto University Ohjelmointi 1 MOOC.
I've had an idea of using a declarative language to describe some sort of simulation game,
so this seemed like a good opportunity to build a prototype.

Building
--------

The game engine is built using Scala with the IDEA build system (I couldn't get *sbt* working from
both Eclipse and IDEA). The directory structure also contains an Eclipse export on top of the IDEA
project, so it should be openable in either IDE. The only dependency the program has is
`scala-swing_2.12-2.0.0` which has been placed as a *.jar*-file to the *bin/* folder, it should be
configured to the build already but you can add it manually to the build if it doesn't work for some
reason.

When building use either `GameGui` or `GameStdio` as the main class. I recommend the GUI though since,
it's a lot more responsive, supports text styles and ephemeral messages from the game (more on it below).
The game needs to have the *script/* folder in it's working directory (Note: the game attempts to search
it from parent folders so it should work with both Eclipse and IDEA default working directories)

Code structure
--------------

The code is organized into packages, roughly in dependency order: *db*, *vm*, *lang*, *game*, *ui*, *test*.

### *db* - Database

This package implements a queryable "database" engine.
The core functionality of it is `db.Table` - a table that can hold rows of data with per-table fixed amount
of columns. The tables support fast query/update/remove operations with any number of wildcards.

Some example queries:
```
table =
	10, "Hello"
	10, "World"
	15, "Hello"
	20, "Aalto"

query(?, ?) =
	10, "Hello"
	10, "World"
	15, "Hello"
	20, "Aalto"

query(?, "Hello") =
	10, "Hello"
	15, "Hello"

query(15, "Hello") =
	15, "Hello"

query(20, "Hello") =
	(empty)
```

The table also comes in an unordered variant `db.UnorderedTable`, which supports
sets of columns that are mutually unordered. This comes with a high performance
penalty, so there is an optimized `db.UnorderedTableSimple` which only supports
one pair of unordered columns.

Example of a table of unordered pairs:
```
table =
	"From", "To"
	"To", "What"

query(?, ?) =
	"From", "To"
	"To", "What"

query("To", ?) =
	"To", "From"
	"To", "What"

query(?, "To") =
	"From", "To"
	"What", "To"

query("To", "From") =
	"To", "From"
```

### *vm* - Virtual Machine

The virtual machine runs on tables from *db*. Most of the work is done by
`vm.Rule`, which allows combining queries from multiple tables aggregating
all the matching values into one result. Rules can also have actions that
can be executed for the matched values. Note that the querying is separated
from the execution, which allows the high-level usage code to decide which
actions to run from a potentially large set of possibilities.

Example:
```
# A table that associates an object (left) to a place (right)
room =
	"Player", "Hallway"
	"Barrel", "Hallway"
	"Key",    "Hallway"
	"Spoon",  "Hallway"
	"Bench",  "Classroom"
	"Desk",   "Classroom"
	"Pencil", "Classroom"

# A table that lists which objects are items
item =
	"Key"
	"Spoon"
	"Pencil"

# Now say, we want to find all the items in the same room as
# the player, we can do the following query:
query =
	item.query(Item)
	# Here, `Item` has the value of _all_ the items in the table!

	room.query("Player", RoomOfPlayer)
	# As well here `RoomOfPlayer` has _all_ the matching rows in
	# the table `room`, but there is only one.

	room.query(Item, RoomOfPlayer)
	# Now the final step, at this point `Item` and `RoomOfPlayer` are
	# both bound to a set of values, this checks the table `room` for
	# each combination lets only items in the correct room pass.

# The result is all the combinations of valid bound values to
# all of the variables, though here the `RoomOfPlayer` isn't
# very interesting.
result =
	"Key",   "Hallway"
	"Spoon", "Hallway"
```

Rules may have also negated conditions that reject patterns which match
the query.

Another thing to note is the `vm.Entity` class - it's almost empty! Entities
can be instantiated from the scripting language and put in tables, that's it.
They are like unique ID:s that bind tables to together. Game objects are entities,
types in the game are entities, even commands are entities!

The entities could also be represented with unique numbers or whatever, but
a dedicated entity class with a debug name is a bit more understandable.

Note: This package also defines `vm.Table` which is kind of a factory-wrapper
for `db.Table`. It chooses the right table type and enforces some constraints
that can be defined for the columns.

### *lang* - Language

This is the programming language implementation that drives the *vm* layer. The
actual game code is contained in the *script/* folder and written in this language.
It is implemented with fairly straightforward tokenizer and recursive descent parser.
Scala was really well fit for writing parsers as accepting tokens could be done with
partial functions defined only for the set of acceptable tokens!

The language itself simple enough that the [EBNF][wiki-ebnf] can be inlined here:

```ebnf
program = top-level*
        | <eof> ;

top-level = "table" ex-name ID* query-body?
          | "entity" ex-name query-body?
          | "define" ex-name query-tuple
          | "external" ex-name STRING
          | ex-name "{" top-level* "}"
          | query-stmt
          | <newline> ;

query-stmt  = ex-name ":"? query-tuple ("," query-tuple)* ;
query-tuple = expr* ;
query-body  = "{" ( "!"? query-stmt* | <newline> ) "}" ;

expr      = STRING | NUMBER | ex-name | ex-lambda | ex-value ;
ex-name   = ID ( "." ID )* ;
ex-lambda = "(" ID* ")" query-body? ("->" query-body)? ;
ex-value  = "*" ex-name ;
```

### *game* - Game Engine

Since most of the work is done by the virtual machine and database
engines.

### *ui* - User Interface

### *test* - Test

[wiki-ebnf]: https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form

