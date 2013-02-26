// environment=browser

// body(0): Element
var body = window.document.body;

// newElt(0): Element
var newElt = document.createElement("div");

// newElt_style_border: string
var newElt_style_border = newElt.style.border;

// e_which: number
var e_which;
window.addEventListener("mousemove", function(e) { e_which = e.which; });
