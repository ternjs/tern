// environment=browser

async function aFetch () {
  var res = await fetch('htts://www.google.com');
  var data = await res.arrayBuffer();
  data; //: ArrayBuffer
  return data;
}

async function aParse () {
  var tmp = await 100;
  tmp; //: number
  var arr = await aFetch();
  arr; //: ArrayBuffer
}


async function bFetch () {
  var res = await fetch('htts://www.google.com');
  return res.arrayBuffer();
}

async function bParse () {
  var arr = await bFetch();
  arr; //: ArrayBuffer
}


async function cFetch () {
  var res = await fetch('htts://www.google.com');
  return res.arrayBuffer();
}

async function cParse () {
  var pr = cFetch();
  pr; //: Promise
  pr.then(function (arr) {
    arr; //: ArrayBuffer
  });
}
