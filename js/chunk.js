"use strict";
var $S = require('suspend'), $R = $S.resume, $T = function(gen) { return function(done) { $S.run(gen, done); } };


module.exports = function(prolog) {
    this._prolog = prolog;
};
var clazz = module.exports.prototype;

clazz.init = function(patch, cb) {
    var em = this._prolog.request('create(' + patch + ')');
    em.on('success', function(v0) {
	cb(undefined, v0);
    });
    em.on('error', function(err) {
	cb(err);
    });
};