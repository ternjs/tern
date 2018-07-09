// environment=browser

window.document.body; //: Element

var newElt = document.createElement("div"); //: Element

newElt.style.border; //: string

var e_which;
window.addEventListener("mousemove", function(e) { e_which = e.which; });
e_which; //: number

console.; //+ assert, clear, count, debug, dir, error, group, groupCollapsed, groupEnd, info, log, table, time, timeEnd, trace, warn

var u = new URL();
u.; //+ hash, host, hostname, href, origin, password, pathname, port, protocol, search, searchParams, username
u.origin; //: string

var sq = u.searchParams;
sq; //: URLSearchParams
sq.; //+ append, delete, entries, get, getAll, has, keys, set, sort, toString, values
sq.keys().next().value; //: string