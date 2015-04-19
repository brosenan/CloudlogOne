"use strict";
var EventEmitter = require('events').EventEmitter;
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

clazz.apply = function(v1, patch) {
    var em1 = this._prolog.request('on((' + v1 + '), ' + patch + ')');
    var em2 = new EventEmitter();
    forwardEvent('success', em1, em2);
    return em2;
};

function forwardEvent(ev, from, to) {
    from.on(ev, function(data) {
	to.emit(ev, data);
    });
}