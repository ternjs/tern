// environment=browser

var aFetch = async () => {
  var res = await fetch('htts://www.google.com');
  var data = await res.arrayBuffer();
  data; //: ArrayBuffer
  return data;
}

var aParse = async () => {
  var tmp = await 100;
  tmp; //: number
  var arr = await aFetch();
  arr; //: ArrayBuffer
}


var bFetch = async () => {
  var res = await fetch('htts://www.google.com');
  return res.arrayBuffer();
}

var bParse = async () => {
  var arr = await bFetch();
  arr; //: ArrayBuffer
}


var cFetch = async () => {
  var res = await fetch('htts://www.google.com');
  return res.arrayBuffer();
}

var cParse = async () => {
  var pr = cFetch();
  pr; //: Promise
  pr.then(function (arr) {
    arr; //: ArrayBuffer
  });
}
