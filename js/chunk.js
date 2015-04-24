"use strict";
var EventEmitter = require('events').EventEmitter;
var $S = require('suspend'), $R = $S.resume, $T = function(gen) { return function(done) { $S.run(gen, done); } };
var vercast = require('vercast');
var asyncgen = require('asyncgen');
var FireAndForget = require('./fireAndForget.js');

module.exports = function(prolog, upstream, bucketStore) {
    this._prolog = prolog;
    this._upstream = upstream;
    this._bucketStore = bucketStore;
};
var clazz = module.exports.prototype;

clazz.init = function(patch, cb) {
    var fnf = new FireAndForget();
    var em = this._request('create(' + patch + ')', fnf);
    var cb1 = fnf.fork();
    var res;
    em.on('success', function(v0) {
	res = v0;
	cb1();
    });
    em.on('error', function(err) {
	cb1(err);
    });
    fnf.join(function(err) {
	cb(err, res);
    });
};

clazz.apply = function(v1, patch) {
    var self = this;
    var em2 = new EventEmitter();
    var patchesIn = [];
    var patchesOut = [];
    var fnf = new FireAndForget();
    $S.run(function*() {
	var em1 = self._request('on((' + v1 + '), ' + patch + ')', fnf);
	em1.on('upstream', function(v, k, p) {
	    patchesOut.push({v: v, k: k, p: p});
	});
	forwardEvent('downstream', em1, em2);
	var v2 = (yield em1.on('success', $S.resumeRaw()))[0];
	patchesOut.forEach(function(pair) {
	    self._upstream.apply(pair.v, pair.p, $S.fork());
	});
	var newIDs = yield $S.join();
	for(let i = 0; i < newIDs.length; i++) {
	    let patch;
	    if(patchesOut[i].v.substring(patchesOut[i].v.length-4) === ",'_'") {
		patch = 'h_putPlaceholder(' + patchesOut[i].k + ',(' + newIDs[i] + '))';
	    } else {
		patch = 'h_updatePlaceholder(' + patchesOut[i].k + ',(' + patchesOut[i].v + '),(' + newIDs[i] + '))';
	    }
	    let em = self._request('on((' + v2 + '), ' + patch + ')', fnf);
	    v2 = (yield em.on('success', $S.resumeRaw()))[0];
	}
	yield fnf.join($R());
	em2.emit('success', v2);
    });
    return em2;
};

function forwardEvent(ev, from, to) {
    from.on(ev, function(data) {
	to.emit(ev, data);
    });
}

clazz._request = function(op, fnf) {
    var self = this;
    var em = this._prolog.request(op);
    em.on('persist', function(id, op) {
	if(!fnf) return;
	asyncgen.async(function*() {
	    yield* self._bucketStore.append(id, [op]);
	})(fnf.fork());
    });
    return em;
};

clazz.open = function(id, cb) {
    var self = this;
    $S.run(function*() {
	var bucket = yield asyncgen.async(function*() {
	    return yield* self._bucketStore.retrieve(id);
	})($R());
	for(let i = 0; i < bucket.length; i++) {
	    let em = self._request(bucket[i]);
	    yield em.on('done', $R());
	}
    }, cb);
};

