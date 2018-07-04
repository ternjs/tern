// environment=browser

var f = fetch('htts://www.google.com');

f.then(function (resp) {
  resp; //: Response

  resp.; //+ arrayBuffer, blob, bodyUsed, clone, error, formData, headers, json, ok, redirect, status, statusText, text, type, url

  resp.arrayBuffer().then(function (ab) {
    ab; //: ArrayBuffer
  });
  resp.blob().then(function (b) {
    b; //: Blob
  });
  resp.json().then(function (j) {
    j; //: ?
  });
  resp.text().then(function (t) {
    t; //: string
  });
});
