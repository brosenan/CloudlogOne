"use strict";
var assert = require('assert');
var $S = require('suspend'), $R = $S.resume, $T = function(gen) { return function(done) { $S.run(gen, done); } };
var request = require('request');
var vercast = require('vercast');

var LogicNode = require('../js/logicNode.js');

function bucketStore() {
    var bs = new vercast.DummyBucketStore();
    return bs;
}

describe('LogicNode', function(){
    describe('LogicNode(options, bucketStore)', function(){
	describe('port', function(){
	    it('should specify the port to be opened', $T(function*(){
		var node = new LogicNode({port: 12345}, bucketStore());
		yield node.start($R());
		var resp = yield request('http://localhost:12345/foobar', $S.resumeRaw());
		assert.ifError(resp[0]);
		assert.equal(resp[1].statusCode, 404);
		yield node.stop($R());
		resp = yield request('http://localhost:12345/foobar', $S.resumeRaw());
		assert.equal(resp[0].code, 'ECONNREFUSED');
	    }));
	});
	describe('peer', function(){
	    it('should specify the URL of a known peer', $T(function*(){
		var node1 = new LogicNode({port: 12001}, bucketStore());
		yield node1.start($R());
		var node2 = new LogicNode({port: 12002,
					   peer: 'http://localhost:12001'},
					  bucketStore());
		yield node2.start($R());
		assert.equal(node1._locator.knownPeers().length, 2);
		yield node1.stop($R());
		yield node2.stop($R());
	    }));
	});
	describe('clusterSize', function(){
	    it('should specify the expected size of the cluster (number of peers)', function(){
		var node = new LogicNode({clusterSize: 333}, bucketStore());
		assert.equal(node._locator._nodehash.size, 333);
	    });
	});
	describe('capacity', function(){
	    it('should specify the maximum number of chunks to be stored in this node at any given time', function(){
		var node = new LogicNode({capacity: 333}, bucketStore());
		assert.equal(node._chunkStore._cache.max, 333);
	    });
	});
	describe('maxDepth', function(){
	    it('should specify the maximum depth of each chunk', $T(function*(){
		var node = new LogicNode({maxDepth: 444}, bucketStore());
		var em = node._chunkStore._prolog.request('set_max_depth(3)');
		var res = yield em.on('success', $S.resumeRaw());
		assert.equal(res[0], 'was: 444');
	    }));
	});
    });
    describe('.start(cb(err))', function(){
    });
    describe('.stop(cb(err))', function(){
    });
    describe('REST API', function(){
	var node;
	beforeEach($T(function*(){
	    node = new LogicNode({port: 12001}, bucketStore());
	    yield node.start($R());
	}))
	afterEach($T(function*(){
	    yield node.stop($R());
	}));
	describe('POST /new', function(){
	    it('should accept a patch and create a new chunk with the given ID', $T(function*(){
		var postOpts = {
		    method: 'POST',
		    uri: 'http://localhost:12001/new',
		    json: true,
		    body: {id: 'foo', patch: 'add_v((a(3):-true), 1)'},
		};
		var resp = yield request(postOpts, $S.resumeRaw());
		assert.ifError(resp[0]);
		assert.equal(resp[1].statusCode, 200);
		// Should return the new version ID
		assert.equal(typeof resp[2].ver, 'string');
		// The internal and external IDs should match
		var m = resp[2].ver.split(',');
		assert.equal(m[0], m[1]);
	    }));
	});
	var initChunk = $S.async(function*(port, patch) {
	    port = port || 12001;
	    patch = patch || 'add_v((a(3):-true), 1)';
	    var postOpts = {
		method: 'POST',
		uri: 'http://localhost:' + port + '/new',
		json: true,
		body: {id: 'foo', patch: patch},
	    };
	    var resp = yield request(postOpts, $S.resumeRaw());
	    assert.ifError(resp[0]);
	    return resp[2].ver;
	});
	describe('POST /apply', function(){
	    it('should apply the given query on the given version ID', $T(function*(){
		var ver = yield initChunk($R());
		// Apply a patch
		var postOpts = {
		    method: 'POST',
		    uri: 'http://localhost:12001/apply',
		    json: true,
		    body: {ver: ver, patch: 'add_v((a(4):-true),2)'},
		};
		var resp = yield request(postOpts, $S.resumeRaw());
		assert.ifError(resp[0]);
		assert.equal(resp[1].statusCode, 200);
		ver = resp[2].ver;
		// Perform a query
		postOpts = {
		    method: 'POST',
		    uri: 'http://localhost:12001/apply',
		    json: true,
		    body: {ver: ver, patch: 'logicQuery(X, a(X), 1)'},
		};
		var resp = yield request(postOpts, $S.resumeRaw());
		assert.ifError(resp[0]);
		assert.equal(resp[1].statusCode, 200);
		assert.equal(resp[2].res[0], 'res(3,1)');
		assert.equal(resp[2].res[1], 'res(4,2)');
	    }));
	    var applyPatch = $S.async(function*(port, ver, patch) {
		var postOpts = {
		    method: 'POST',
		    uri: 'http://localhost:' + port + '/apply',
		    json: true,
		    body: {ver: ver, patch: patch},
		};
		var resp = yield request(postOpts, $S.resumeRaw());
		assert.ifError(resp[0]);
		assert.equal(resp[1].statusCode, 200);
		return resp[2].ver;
	    });
	    var runQuery = $S.async(function*(port, ver, q) {
		var postOpts = {
		    method: 'POST',
		    uri: 'http://localhost:' + port + '/apply',
		    json: true,
		    body: {ver: ver, patch: q},
		};
		var resp = yield request(postOpts, $S.resumeRaw());
		assert.ifError(resp[0]);
		assert.equal(resp[1].statusCode, 200);
		return resp[2].res;
	    });
	    it('should forward requests to other nodes if needed', $T(function*(){
		var bs = bucketStore();
		var n1 = new LogicNode({port: 12002, maxDepth: 1}, bs);
		yield n1.start($R());
		var n2 = new LogicNode({port: 12003, maxDepth: 1, peer: 'http://localhost:12002'}, bs);
		yield n2.start($R());
		var v = yield initChunk(12002, 'add_v((a(2):-true),1)', $R());
		v = yield applyPatch(12002, v, 'add_v((a(1):-true),1)', $R());
		v = yield applyPatch(12002, v, 'add_v((a(3):-true),1)', $R());
		var res = yield runQuery(12002, v, 'logicQuery(X, a(X), 1)', $R());
		assert.equal(res.length, 3);
		yield n1.stop($R());
		yield n2.stop($R());
	    }));
	    it('should use local chunks if peers are  not available', $T(function*(){
		var bs = bucketStore();
		var n1 = new LogicNode({port: 12002, maxDepth: 1}, bs);
		yield n1.start($R());
		var v = yield initChunk(12002, 'add_v((a(2):-true),1)', $R());
		v = yield applyPatch(12002, v, 'add_v((a(1):-true),1)', $R());
		v = yield applyPatch(12002, v, 'add_v((a(3):-true),1)', $R());
		var res = yield runQuery(12002, v, 'logicQuery(X, a(X), 1)', $R());
		assert.equal(res.length, 3);
		yield n1.stop($R());
	    }));
	});
    });
});
