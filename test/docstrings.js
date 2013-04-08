Date; //~ Creates JavaScript Date instances which let you work with dates and times.

var myalias = Date;

myalias; //~ Creates JavaScript Date instances which let you work with dates and times.

// This is variable foo.
var foo = 10;

foo; //~ This is variable foo.

// This function returns a monkey.
function makeMonkey() { return "monkey"; }

makeMonkey; //~ This function returns a monkey.

var monkeyAlias = makeMonkey;

monkeyAlias; //~ This function returns a monkey.

// This is an irrelevant comment.


// This describes abc.
var abc = 20;

abc; //~ This describes abc.

// Quux is a thing. And here are a bunch more sentences that would
// make the docstring too long, and are thus wisely stripped by Tern's
// brain-dead heuristics. Ayay.
function Quux() {}

Quux; //~ Quux is a thing.

/*  Extra bogus 
 *  	whitespace is also stripped.
 */
var baz = "hi";

baz; //~ Extra bogus whitespace is also stripped.

var o = {
  // Get the name.
  getName: function() { return this.name; },
  // The name
  name: "Harold"
};

o.getName; //~ Get the name.
o.name; // The name
