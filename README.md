Abenteuer
=========

Text adventure for Aalto University Ohjelmointi 1 MOOC.
I've had an idea of using a declarative language to describe some sort of simulation game,
so this seemed like a good opportunity to build a prototype.

There's a live version of the game at [http://ab.dorfbook.com][ab-dorfbook]

Building
--------

The game engine is built using Scala with the IDEA build system (I couldn't get *sbt* working from
both Eclipse and IDEA). The directory structure also contains an Eclipse export on top of the IDEA
project, so it should be openable in either IDE. The only dependency the program has is
`scala-swing_2.12-2.0.0` which has been placed as a *.jar*-file to the *bin/* folder, it should be
configured to the build already but you can add it manually to the build if it doesn't work for some
reason.

When building use either `GameGui`, `GameStdio` or `GameServer` as the main class. I recommend the GUI though since,
it's a lot more responsive, supports text styles and ephemeral messages from the game (more on it below).
The game needs to have the *script/* folder in it's working directory (Note: the game attempts to search
it from parent folders so it should work with both Eclipse and IDEA default working directories)

Code structure
--------------

The code is organized into packages, roughly in dependency order: *util*, *db*, *vm*, *lang*, *game*, *ui*, *test*.

### *util* - Utilities

A mixed bag of useful things, mostly special-cased iterators and string manipulation. The most useful feature of
this package is `SimpleIterator[T]` which wraps Scala's `Iterator[T]` interface merging the two calls:

```scala
// Scala iterator, you need to spread the iterator functionality into two
// methods, which in many cases is inconvenient.
class Iterator[T] {
	def hasNext: Boolean
	def next(): T
}

// Return `Some(value)` if there are still things left to iterate, `None`
// otherwise, simple as that! Also, after returning `None` once the method
// is _guaranteed_ to never be called again! (it will throw as this is illegal,
// analogous to calling `next()` when `hasNext = false`)
class SimpleIterator[T] {
	def nextOption(): Option[T]
}
```

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
engines, most that this package has to do is to gather all the source
files to compile and run the high-level game engine logic, such as
what rules to query and how to execute their results. Since there is
only one API method, we can trivially turn this into an RPC system by
serializing the input and output somehow. An example of this is
`GameServer` which exposes the game as a simple HTML server.

### *ui* - User Interface

The actual user-visible portion of the project. Contains three
alternative frontends for the game: one text-based, one GUI and one
remote. Detailed descriptions of the frontends:

#### *GameStdio*

Probably the simplest frontend you could implement for the game.
Doesn't support any fancy features like ephemeral messages or formatting.

Actually it's so short I may as well inline the whole implementation here:
```scala
package ui

object GameStdio extends App {
  val theGame = new game.Game()

  def appendSpans(spans: Seq[TextSpan]) = spans.foreach(span => print(span.text))

  val hello = theGame.interact("/hello")
  appendSpans(hello.spans)

  while (true) {
    Console.print("\n> ")
    Console.flush()
    val line = scala.io.StdIn.readLine()

    val result = theGame.interact(line)
    // In case the game wants to override the user prmpt, print it!
    // Sadly, we can't replace already printed lines of the standard output.
    result.overridePrompt.foreach(prompt => println(s"> $prompt"))
    appendSpans(result.spans)
  }
}
```

#### *GameGui*

A Java Swing based GUI frontend which supports all the fancy features.
This is the only thing that depends on the Swing *.jar* so if there is no way
to get it working you should be able to just remove `GameGui.scala` from the
build and carry on.

![Screenshot of GameGui](https://github.com/bqqbarbhg/abenteuer/raw/master/gui-screenshot.png)

#### *GameServer*

Half of the remote client. The other half is located at *ab-html/index.html*.
It should work with any modern browser that supports `fetch` (Chrome tested).
The code is at most a proof-of-concept and since there was no real HTTP server
in the standard library it's kind of a mess. The Javascript side also didn't
receive much love and probably bugs out if stressed too much. But as a concept
it works pretty well.

This idea could be improved further, and by extending the game API with some
kind of per-user token it should be pretty easy to make a multiplayer text
adventure with this system, where each player controls a different character.

### *test* - Test

This package contains a collection of test files (not unit tests!) that were
used during the development of the features.

In chornological order:
* **TableTest** - Put some stuff in `db.Table` and query it
* **QueryTest** - Create a `vm.Rule` to show items in an inventory
* **ScannerTest** - Tokenize a small source file
* **ParserTest** - Parse a small source file and dump the AST
* **CodegenTest** - Actually compile two source files and execute one rule!

Design goals
------------

I wanted to fix some of the things that annoyed me in text adventures.

### Ephemeral messages

Ephemeral messages are my solution to the log spam that occurs from trying
out different things in a text adventure. For example something like:

```
> lift rokc
I don't find anything to lift
> lift rock
I don't find anything to lift
> lift stone
You lift stone
```

In Abenteuer, when a command fails the output is marked as **ephemeral**.
This in itself doesn't do anything else but to inform the frontend that
the result of the command isn't all that interesting. Then if the frontend
supports it (currently GUI and HTML) the printed message will be written
over by the next command. This goes hand-in-hand with *target selection*.

### Target selection

There are times when it's not totally obvious what to write so the game
does what you want. The original idea was to resolve ambiguous actions
with a list:
```
> inspect barrel
Select by typing 1-2
1. Barrel
2. Barrel (shiny)
> 2
A shiny barrel that gleams in the sunlight.
```

Then the idea expanded to allowing to omit the "argument" to the command
completely like:
```
> inspect
Select by typing 1-3
1. Barrel
2. Barrel (shiny)
3. Key
4. Spoon
> 3
A key that opens doors or something.
```

I think this makes playing the game a more enjoyable interactive experience
since you spend more time solving the high-level puzzles rather than fighting
with the parser.

Language reference
------------------

The game itself (found under *script/*) is written in the custom scripting
language implemented by *lang*. There is a shoddy [Vim][vim] syntax definition
for the language in the root called *abenteuer.vim*.

### Syntax basics

The langauge is newline-terminated, but you can wrap a line with `\` and split
it using `;`, for example the three snippets are equivalent:
```
first line
second

first \
line
second

first line; second
```

Basic lexical elements:
```
asd my-thing # Identifier (may have dashes)
123 -5       # Number
"Hello!" ""  # String
table entity # Keyword
```

### Tables

Tables are the most fundamental thing of the langauge and engine. They are
lightweight but powerful datastructures that can contain values and be queried.
The columns of a table can be given constraints while defining the table.

```
# Declares a table called my-table with two columns
table my-table col1 col2

table name Self Name {
	# Column Self values must be unique -> can only have one name
	unique Self
}
```

### Entities

Entities are like unique objects that can be added to tables.
They don't really have any properties of their own except a debug name.
All other data must be specified using tables.

```
# Declares a new entity called barrel
entity Barrel

# Now, we can add properties by inserting them into tables,
# the syntax is <table-name> <col1> <col2>...
name Barrel "Awesome Barrel"
desc Barrel "This is a barrel containing mysteries"
age  Barrel 15

# Since it's very common to have the entity be the first column in a table
# there is a shorthand for it:
entity Other-Barrel {
	name "Boring Barrel"
	desc "It looks empty"
	age  22
}
```

There is a syntactic sugar that allows writing repeated statements more
palatable. You can do multiple statements using a comma between the arguments.

```
entity Player {
	keyword "me", "self", "player"
}
# Is same as:
entity Player {
	keyword "me"
	keyword "self"
	keyword "me"
}
# Is same as:
entity Player
keyword Player, "me"
keyword Player, "self"
keyword Player, "me"
# Also could be:
keyword Player "me", Player "self", Player "me"
```

### Namespaces

All the names in the script are in the same namespace, but you can define your
own namespaces.

```
# Declares an entity under the namespace example
entity example.thing

# You can also open a namespace block
example {
	# Declares `example.other`
	entity other

	# Declares `example.nested.other`
	entity nested.other

	# Tables can also be namespaced
	table property self
}
```

### Lambdas

The above langauge features already allow for defining almost everything you need to.
However there is no way to make *behavior* with it yet. This is where the most
complicated feature of the language comes in: lambdas.

The syntax looks like this:
```
(arg1 arg2) { query } -> { action }

# If either the query or action part is empty it
# can be omitted:
	(arg) { query } -> { }
	(arg) { query }

	(arg) { } -> { action }
	(arg) -> { action }
```

By default all query and action identifier create a new local variable to bind to.
If you want to refer to an existing entity you must prefix the name with `*`.
```
table has self thing

entity Player
entity Goblin
entity Club
entity Sword

has Goblin Club
has Player Sword

() {
	# Matches all possible pairs of table `has`:
	has Player Thing
	# -> Player=Player Thing=Sword
	# -> Player=Goblin Thing=Club
}

() {
	# Matches only things `entity Player` has.
	has *Player Thing
	# -> Player=Player Thing=Sword
}
```

The query and action syntax is pretty much equivalent, but they do opposite things.
Queries filter based on the constraints, actions make the constraints happen.

```
() {
	# Queries for items that player has
	has *Player Thing
	# Filters query to only non-shiny things
	! shiny Thing
}

(Item) -> {
	# Adds item to player's inventory
	has *Player Item
	# Removes `Thing` from the table `shiny`
	! shiny Thing
}
```

The action block also can do other things than update the tables. Most useful
is calling into the engine code to do something. First, the command must be
imported using the `external` keyword.

```
# Load the external function "game-print" to `print`
external print "game-print"

(Door) {
	locked Door
} -> {
	print: "{Door} is locked!"
}
```

The thing that empowers this language is that the lambdas are a first class value
and can be stored in tables!

For example defining a behavior for a command:
```
entity xyzzy {
	cmd.keyword "xyzzy"

	cmd.do () -> {
		print: "Nothing happens..."
	}
}

# Now there is a row in the table `cmd.do` containing
(Entity(xyzzy), Lambda(-> print..))
```

Game engine
-----------

The language specification still isn't enough to make a game. The script
needs to communicate with the engine somehow and, you guessed it, it's
through tables! You can look at *script/engine.abt* for the basic definitions
of the engine.

Let's for example go through the implementation of the `inspect` command found at
*script/command/inspect.abt*. All the command-related tables are in the namespace
`cmd`.

First we need to define an entity for the command that binds it together:
```
entity inspect {
```

Then let's define some properties, when the user types something the game will
query these tables to find a matching command. We can also write a short help
description here.
```
	cmd.keyword "inspect", "stare at"
	cmd.abbrev "i" "inspect"
	cmd.help "**inspect**: Inspect finer details of objects"
```

Now for the actual behavior of the command. This is split into two functions
the engine will call. First it checks if there exists a `cmd.select` for the
command. Select should query all the objects the command is currently applicable
to. Then the player can select one either using the `keyword` table or choosing
from a list. 

```
	cmd.select (Thing) {
		# This will match every object that has the same `room` as the player
		room *Player Room
		room Thing Room
		object Thing
	}
```

Note that in select the argument is actually a **wildcard**, not an entity.
This is kind of weird language in the sense that to return values from functions
you actually give them arguments that are not defined and the function will
define them using constraints.

The actual command execution is defined in `cmd.do`. The querying is only possible
in the query-block, so we find the name and description for the object there.
```
	# This is called with Thing actually bound to something!
	cmd.do (Thing) {
		name Thing Name # Find the matching name
		desc Thing Desc # Find the matching desc
	} -> {
		# Print them!
		print: "**{Name}**"
		print: Desc
	}
```

Note that if either of the columns would have multiple entries for the entity
the name-description pair would be printed multiple times! Also if one of them
doesn't exist we don't print anything! We can avoid this by splitting the command
into two parts:
```
	cmd.do (Thing) { name Thing Name } -> { print: "**{Name}**" }
	cmd.do (Thing) { desc Thing Desc } -> { print: Desc } 10
```

The number `10` after the second lambda is a value for an optional column of the
`cmd.do` table which defines the sort order. The default value is `0`. Before
executing commands the engine sorts the possible actions in increasing priority
order.

You can also break out of commands with the `fail:` action. This allows adding
failure cases as high-priority actions. In the following example if you try
to pick up something that is in the table `hot` it will print a message and
not execute any further actions, since it has a lower priority than the default
(-100 < 0) it will interrupt the actual action.

```
cmd.do take (Thing) -> {
	has *Player Thing
	print: "Picked up {Thing}"
}

# This can also be defined in another file or anywhere
cmd.do take (Thing) {
	hot Thing
} -> {
	print: "You burn your fingers on {Thing}"
	fail:
} -100
```

Postmortem
----------

The development was extremely fast with the */reload* and */replay* commands
that recompile the game and respectively restart the game or play all your
previous moves to get you back to the state before restarting. This shortens
the iteration time to a minimum.

There are some pretty obvious flaws with the scripting system, as it's still
a little too rigid. If I had time to properly implement calling functions from
the script it would have helped a lot I think.

Also there is no real way to express things like counting or sorting/ordering
thing _in the script_.

There should probably be a way to override tables with functions. For example
if there is a guard guarding a room it may be expressed as:
```
guard Enemy Room

cmd.do command.go (Link) {
	guard Guard Link Message
} -> {
	print: "**{name:Guard}**: {Message}"
	fail:
} -10
```
There is no way to do it non-locally, if we would add some *distracted* table
it would add state to toggle which is no good.

One solution to this would be some kind of new `condition` structure that you
can override from other places.

Another problem is the code duplication in the split *select*/*do* system for
the commands. There really should be some way to pass data between those two
phases.

[vim]: http://vim.org
[wiki-ebnf]: https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form
[ab-dorfbook]: http://ab.dorfbook.com

