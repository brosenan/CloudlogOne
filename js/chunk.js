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

clazz.apply = function(v1, patch, downCB, cb) {
    var self = this;
    var upstream = {};
    var upstreamKeys = {};
    var fnf = new FireAndForget();
    $S.run(function*() {
	var em1 = self._request('on((' + v1 + '), ' + patch + ')', fnf);
	em1.on('upstream', function(v, k, p) {
	    if(!(v in upstream)) {
		upstream[v] = [];
	    }
	    upstream[v].push(p);
	    upstreamKeys[v] = k;
	});
	em1.on('downstream', function(res) {
	    downCB(res);
	});
	em1.on('error', function(err) {
	    cb(err);
	});
	var v2 = (yield em1.on('success', $S.resumeRaw()))[0];
	var placeholders = Object.keys(upstream);
	placeholders.forEach(function(v) {
	    self._upstream.apply(v, '[' + upstream[v].join(',') + ']', $S.fork());
	});
	var newIDs = yield $S.join();
	for(let i = 0; i < newIDs.length; i++) {
	    let patch;
	    if(placeholders[i].substring(placeholders[i].length-4) === ",'_'") {
		patch = 'h_putPlaceholder(' + upstreamKeys[placeholders[i]] + ',(' + newIDs[i] + '))';
	    } else if(placeholders[i] !== newIDs[i]) {
		patch = 'h_updatePlaceholder(' + upstreamKeys[placeholders[i]] + ',(' + placeholders[i] + '),(' + newIDs[i] + '))';
	    }
	    if(patch) {
		let em = self._request('on((' + v2 + '), ' + patch + ')', fnf);
		em.on('error', function(err) {
		    cb(err);
		});
		v2 = (yield em.on('success', $S.resumeRaw()))[0];
	    }
	}
	yield fnf.join($R());
	return v2;
    }, cb);
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
	    em.on('error', function(err) { cb(err); });
	    yield em.on('success', $S.resumeRaw());
	}
    }, cb);
};

clazz.setUpstream = function(upstream) {
    this._upstream = upstream;
};