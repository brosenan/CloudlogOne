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
	describe('POST /new/:id', function(){
	    it('should accept a patch and create a new chunk with the given ID', $T(function*(){
		var postOpts = {
		    method: 'POST',
		    uri: 'http://localhost:12001/new/foo',
		    json: true,
		    body: ['add_v((a(3):-true), 1)'],
		};
		var resp = yield request(postOpts, $S.resumeRaw());
		assert.ifError(resp[0]);
		// Should return the new version ID
		assert.equal(typeof resp[2].ver, 'string');
		// The internal and external IDs should match
		var m = resp[2].ver.split(',');
		assert.equal(m[0], m[1]);
	    }));
	});
	var initChunk = $S.async(function*() {
	    var postOpts = {
		method: 'POST',
		uri: 'http://localhost:12001/new/foo',
		json: true,
		body: ['add_v((a(3):-true), 1)'],
	    };
	    var resp = yield request(postOpts, $S.resumeRaw());
	    assert.ifError(resp[0]);
	    return resp[2].ver;
	});
	describe('POST /apply', function(){
	    it('should apply the given query on the given version ID', $T(function*(){
		var ver = yield initChunk($R());
		var postOpts = {
		    method: 'POST',
		    uri: 'http://localhost:12001/apply',
		    json: true,
		    body: {ver: ver, patches: ['logicQuery(X, a(X), 1)']},
		};
		var resp = yield request(postOpts, $S.resumeRaw());
		assert.ifError(resp[0]);
		assert.equal(resp[1].statusCode, 200);
		assert.equal(resp[2].res[0], 'res(3,1)');
	    }));
	});
    });
});
