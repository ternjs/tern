// environment=browser

window.document.body; //: Element
var anyElt = document.createElement("any"); //: Element

var newElt = document.createElement("div"); //: HTMLDivElement

newElt.style.border; //: string

var e_which;
window.addEventListener("mousemove", function(e) { e_which = e.which; });
e_which; //: number
