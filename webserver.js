var express = require('express');
var app = express();

app.get('/', function (req, res) {
    res.send({resp:'Hello, ' + req.ip.match(/[^:]*$/)[0], list: [1, 2, 3]});
});

var server = app.listen(3000, function () {
    var host = server.address().address;
    var port = server.address().port;

    console.log('Example app listening at http://%s:%s', host, port);
});
