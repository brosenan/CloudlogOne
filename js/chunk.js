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
	var ver;
	var clientPatch;
	em.on('success', function(v0) {
		ver = v0;
		cb1();
	});
	em.on('error', function(err) {
		cb1(err);
	});
	em.on('client-patch', function(patch) {
		clientPatch = patch;
	});
	fnf.join(function(err) {
		cb(err, {ver: ver, clientPatches: [clientPatch]});
	});
};

clazz.apply = function(v1, patch, cb) {
	var self = this;
	var upstream = {};
	var upstreamKeys = {};
	var fnf = new FireAndForget();
	var downstream = [];
	var clientPatches = [];
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
			downstream.push(res);
		});
		em1.on('client-patch', function(res) {
			if(res !== '[]') {
				clientPatches.push(res);
			}
		});
		em1.on('error', function(err) {
			cb(err);
		});
		var v2 = (yield em1.on('success', $S.resumeRaw()))[0];
		return yield calcUpstream(v2, upstream, upstreamKeys, $R());
	}, cb);

	var applyPatch = function(ver, patch, fnf, cb) {
		let em = self._request('on((' + ver + '), ' + patch + ')', fnf);
		em.on('error', function(err) {
			cb(err);
		});
		em.on('client-patch', function(res) {
			clientPatches.push(res);
		});
		em.on('success', function(res) {
			cb(undefined, res);
		});
	};

	var calcUpstream = $S.async(function*(v2, upstream, upstreamKeys) {
		var placeholders = Object.keys(upstream);
		placeholders.forEach(function(v) {
			self._upstream.apply(v, '[' + upstream[v].join(',') + ']', $S.fork());
		});
		var placeholderPatches = yield $S.join();
		for(let i = 0; i < placeholderPatches.length; i++) {
			let patch = placeholderPatches[i];
			if(patch) {
				try {
					v2 = yield applyPatch(v2, patch, fnf, $R());
				} catch(e) {
					let m = e.message.match(/treap_error\(unexpected_placeholder\(\((.*)\),[ ]*\((.*)\)\)\)/);
					if(m) {
						v2 = yield self.apply(v2, upstream[placeholders[i]], $R());
					} else {
						throw e;
					}
				}
			}
		}
		yield fnf.join($R());
		return {ver: v2, results: downstream, clientPatches: clientPatches};
	});
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
