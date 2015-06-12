Date; //doc: Creates JavaScript Date instances which let you work with dates and times.
new Date; //doc: Creates JavaScript Date instances which let you work with dates and times.

var myalias = Date;

myalias; //doc: Creates JavaScript Date instances which let you work with dates and times.

// This is variable foo.
var foo = 10;

foo; //doc: This is variable foo.

// This function returns a monkey.
function makeMonkey() { return "monkey"; }

makeMonkey; //doc: This function returns a monkey.

var monkeyAlias = makeMonkey;

monkeyAlias; //doc: This function returns a monkey.

// This is an irrelevant comment.


// This describes abc.
var abc = 20;

abc; //doc: This describes abc.

// Quux is a thing.
// Two lines.
function Quux() {}

Quux; //doc: Quux is a thing.\nTwo lines.

/*
 *  Extra bogus 
 *  whitespace is also stripped.
 */
var baz = "hi";

baz; //doc: Extra bogus\nwhitespace is also stripped.

/* starry format
 * with first line text
 */
var oy = 1;

oy; //doc: starry format\nwith first line text

// Block of text
// With some
//  * indented
//  * pieces
//
// And a blank line
var arr = 6;

arr; //doc: Block of text\nWith some\n * indented\n * pieces\n\nAnd a blank line

var o = {
  // Get the name.
  getName: function() { return this.name; },
  // The name
  name: "Harold"
};

// The string "foo".
o.foo = "foo";

o.getName; //doc: Get the name.
o.name; //doc: The name
o.foo; //doc: The string "foo".
