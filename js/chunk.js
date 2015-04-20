"use strict";
var EventEmitter = require('events').EventEmitter;
var $S = require('suspend'), $R = $S.resume, $T = function(gen) { return function(done) { $S.run(gen, done); } };


module.exports = function(prolog, upstream) {
    this._prolog = prolog;
    this._upstream = upstream;
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
    var self = this;
    var em2 = new EventEmitter();
    var patchesIn = [];
    var patchesOut = [];
    $S.run(function*() {
	var em1 = self._prolog.request('on((' + v1 + '), ' + patch + ')');
	em1.on('upstream', function(v, p) {
	    patchesOut.push({v: v, p: p});
	});
	var v2 = (yield em1.on('success', $S.resumeRaw()))[0];
	patchesOut.forEach(function(pair) {
	    self._upstream.apply(pair.v, pair.p, $S.fork());
	});
	var newIDs = yield $S.join();
	for(let i = 0; i < newIDs.length; i++) {
	    v2 = (yield self._prolog.request('on((' + v2 + '), h_putPlaceholder(' + patchesOut[i].p + ',(' + newIDs[i] + ')))').on('success', $S.resumeRaw()))[0];
	}
	em2.emit('success', v2);
    });
    return em2;
};

function forwardEvent(ev, from, to) {
    from.on(ev, function(data) {
	to.emit(ev, data);
    });
}