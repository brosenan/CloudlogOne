"use strict";
var assert = require('assert');
var http = require('http');
var request = require('request');

var $S = require('suspend'), $R = $S.resume, $T = function(gen) { return function(done) { $S.run(gen, done); } };

var PeerLocator = require('../js/peerLocator.js');

describe('PeerLocator', function(){
    describe('.run(cb(err))', function(){
	
    });

    describe('.app()', function(){
	it('should return an Express app', $T(function*(){
	    var locator = new PeerLocator(5050);
	    locator.app().get('/', function(req, res) {
		res.send('Hello');
	    });
	    // Run the app
	    yield locator.run($R());
	    // Read the response
	    var resp = yield request('http://localhost:5050/', $S.resumeRaw());
	    assert.deepEqual(resp[2], 'Hello');
	}));
    });
    describe('.knownPeers()', function(){
	it('should return a consistent resonse', $T(function*(){
	    var l1 = new PeerLocator(4050);
	    yield l1.run($R());
	    var l2 = new PeerLocator(4051, 'http://localhost:4050');
	    yield l2.run($R());
	    var l3 = new PeerLocator(4052, 'http://localhost:4050');
	    yield l3.run($R());
	    assert.equal(l1.knownPeers().length, 3);
	    assert.equal(l2.knownPeers().length, 3);
	    assert.equal(l3.knownPeers().length, 3);
	    yield l1.stop($R());
	    yield l2.stop($R());
	    yield l3.stop($R());
	}));
    });
    describe('.getServerFor(key)', function(){
	it('should return one of the server URLs for each key, consistently from each locator', $T(function*(){
	    var master = 'http://localhost:4050';
	    var l1 = new PeerLocator(4050, null, 100);
	    yield l1.run($R());
	    var l2 = new PeerLocator(4051, master, 100);
	    yield l2.run($R());
	    var count = 0;
	    for(let i = 0; i < 100; i++) {
		let key = 'foo' + i;
		// The value returned must be consistent
		assert.equal(l1.getServerFor(key), l2.getServerFor(key));
		// Make sure the result is distributed
		if(l1.getServerFor(key) === master) {
		    count += 1;
		}
	    }
	    assert(count > 0, count + ' > 0');
	    assert(count < 100, count + ' < 100');
	    yield l1.stop($R());
	    yield l2.stop($R());
	}));
    });
    describe('.service(path, handler(input, cb(err, output)))', function(){
	it('should register a service to the given path', $T(function*(){
	    var locator = new PeerLocator(4050);
	    locator.service('/test', $S.async(function*(input) {
		return {value: input.value + 2};
	    }));
	    yield locator.run($R());
	    
	    var opts = {
		method: 'POST',
		uri: 'http://localhost:4050/test',
		json: true,
		body: {value: 3},
	    };
	    var resp = yield request(opts, $S.resumeRaw());
	    assert.ifError(resp[0]);
	    assert.equal(resp[1].statusCode, 200);
	    assert.equal(resp[2].value, 5);
	    yield locator.stop($R());
	}));
	it('should handle errors by responding status 500', $T(function*(){
	    var locator = new PeerLocator(4050);
	    locator.service('/test', $S.async(function*(input) {
		throw Error('Foo bar');
	    }));
	    yield locator.run($R());
	    
	    var opts = {
		method: 'POST',
		uri: 'http://localhost:4050/test',
		json: true,
		body: {},
	    };
	    var resp = yield request(opts, $S.resumeRaw());
	    assert.ifError(resp[0]);
	    assert.equal(resp[1].statusCode, 500);
	    assert.equal(resp[2], 'Foo bar');
	    yield locator.stop($R());
	}));
    });
    describe('.request(key, path, input, cb(err, output))', function(){
	it('should call a service', $T(function*(){
	    var handler = $S.async(function*(input) {
		return {value: input.value * 2};
	    });
	    var master = 'http://localhost:4051';
	    var l1 = new PeerLocator(4051, null, 100);
	    l1.service('/foo', handler);
	    yield l1.run($R());

	    var l2 = new PeerLocator(4052, master, 100);
	    l2.service('/foo', handler);
	    yield l2.run($R());

	    var res = yield l1.request('abc', '/foo', {value: 3}, $R());
	    assert.equal(res.value, 6);

	    yield l1.stop($R());
	    yield l2.stop($R());
	}));
	it('should call the handler directly in case no peers are registered', $T(function*(){
	    var handler = $S.async(function*(input) {
		return {value: input.value * 3};
	    });
	    var l1 = new PeerLocator(4055, null, 100);
	    l1.service('/foo', handler);
	    yield l1.run($R());

	    var res = yield l1.request('abc', '/foo', {value: 3}, $R());
	    assert.equal(res.value, 9);

	    yield l1.stop($R());
	}));

    });
});
