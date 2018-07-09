// environment=browser

var theform = new FormData();

var values = theform.values();

var next = values.next();

next.value; //: string
next.done; //: bool

var entries = theform.entries();

var entry = entries.next();

entry.value; //: [number, string|Blob]
entry.done; //: bool
