"use strict";
var express = require('express');

module.exports = function(port) {
    this._app = express();
    this._port = port;
};
var clazz = module.exports.prototype;

clazz.app = function() {
    return this._app;
};

clazz.run = function(cb) {
    this._app.listen(this._port, cb);
};
