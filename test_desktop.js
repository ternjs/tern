var req = require("http").request({
  hostname: "localhost",
  port: Number(require("fs").readFileSync(".tern-port")),
  method: "POST"
}, function(resp) {
  var data = "";
  resp.on("data", function(chunk) { data += chunk; });
  resp.on("end", function() {
    if (resp.status >= 300) {
      console.log("fail " + resp.status + ": " + data);
    } else {
      console.log(data);
    }
  });
});

req.end(JSON.stringify({
  query: {type: "completions",
          file: "infer.js",
          end: 31694}
}));
