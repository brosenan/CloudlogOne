"use strict";
var bodyParser = require('body-parser')
var PeerLocator = require('./peerLocator.js');
var ChunkStore = require('./chunkStore.js');
var PrologInterface = require('./prologInterface.js');
var $S = require('suspend'), $R = $S.resume;
var assert = require('assert');

function UpstreamClient(node, results) {
	this.apply = $S.async(function*(v, patch) {
		var resp = yield this.rawApply(v, patch, $R());
		if(resp.res) {
			resp.res.forEach(function(r) {
				results.push(r);
			});
		}
		return '[' + resp.clientPatches.join(',') + ']';
	});
	this.rawApply = $S.async(function*(v, patch) {
		var split = v.split(',');
		var resp;
		if(split[1] === "'_'") {
			resp = yield node._locator.request(split[0], '/new', {id: split[0],
																  patch: patch}, $R());
		} else {
			resp = yield node._locator.request(split[0], '/apply', {ver: v,
																	patch: patch}, $R());
		}
		if(!resp.res) {
			resp.res = [];
		}
		return resp;
	});
}

var globalQuery = $S.async(function*(self, input, finalResult) {
//	console.log(input);
	var patches = []
	input.patches.forEach(function(patch) {
		if(patch.substring(0,4) === 'add(') {
			patches.push('add_v(' + patch.substring(4));
			patches.push('add_m(' + patch.substring(4));
		} else {
			patches.push(patch);
		}
	});
	var ver;
	if(!input.ver) {
		var em = self._prolog.request('calcInitialHash([' + patches.join(',') + '])');
		ver = yield function(cb) {
			em.on('error', cb);
			em.on('success', function(ver) { cb(undefined, ver + ",'_'"); });
		}($R());
	} else {
		ver = input.ver;
	}
	let client = new UpstreamClient(self);
	var resp = yield client.rawApply(ver, '[' + patches.join(',') + ']', $R());
	var newPatches = [];
	resp.res.forEach(function(res) {
		if(res.substring(0,4) === 'res(') {
			finalResult.push(res);
		} else {
			newPatches.push(res);
		}
	});
	if(newPatches.length === 0) {
		return {ver: resp.ver, results: finalResult};
	} else {
		return yield globalQuery(self, {ver: resp.ver, patches: newPatches}, finalResult, $R());
	}
});
module.exports = function(options, bucketStore) {
	var self = this;

	this._port = options.port;
	this._locator = new PeerLocator(this._port, options.peer, options.clusterSize);
	this._prolog = new PrologInterface();
	// this._prolog = new PrologInterface('logicNode.' + options.port + '.log');
	if(options.maxDepth) {
		this._prolog.request('set_max_depth(' + options.maxDepth + ')');
	}
	this._chunkStore = new ChunkStore(this._prolog, null, bucketStore, options);
	this._locator.service('/new', $S.async(function*(input) {
		var chunk = yield self._chunkStore.getChunk(input.id, $R());
		return yield chunk.init(input.patch, $R());
	}));
	this._locator.service('/apply', $S.async(function*(input) {
		var ver = input.ver;
		var chunk = yield self._chunkStore.getChunk(ver, $R());
		var results = [];
		chunk.setUpstream(new UpstreamClient(self, results));
		var localRes = yield chunk.apply(ver,
								input.patch,
								$R());
		localRes.res = localRes.results.concat(results);
		return localRes;
	}));
	this._locator.service('/', function(input, cb) { globalQuery(self, input, [], cb); });
};

var clazz = module.exports.prototype;

clazz.start = function(cb) {
	return this._locator.run(cb);
};

clazz.stop = $S.async(function*() {
	this._locator.stop($S.fork());
	this._prolog.stop($S.fork());
	yield $S.join();
});
