"use strict";

var assert = require('assert');

var NodeHash = require('../nodehash.js');

describe('NodeHash', function(){
    describe('.hashIndex(key[, rotation])', function(){
	it('should return a uniformly-distributed number (>= 0, < hash-size) based on the key', function(){
	    var hash = new NodeHash(100);
	    var s = 0; 
	    for(var i = 0; i < 100; i++) {
		let index = hash.hashIndex('foo' + i);
		assert.equal(typeof index, 'number');
		assert(index >= 0, index + ' >= 0');
		assert(index < 100, index + ' < 100');
		s += index;
	    }
	    assert(s > 0, s + ' > 0');
	});
	it('should produce different results for different rotation numbers', function(){
	    var hash = new NodeHash(10000);
	    assert.notEqual(hash.hashIndex('foo'), hash.hashIndex('foo', 1));
	});

    });
    describe('.addServer(server)', function(){
	it('should add a server', function(){
	    var hash = new NodeHash(10);
	    hash.addServer('http://127.0.0.1:7777');
	});
    });
    describe('.getServerFor(key)', function(){
	it('should return a server for that key', function(){
	    var hash = new NodeHash(10);
	    hash.addServer('http://127.0.0.1:7777');
	    var server = hash.getServerFor('foo');
	    assert.equal(server, 'http://127.0.0.1:7777');
	});
	it('should choose each server at about the same probability', function(){
	    var hash = new NodeHash(100);
	    var servers = ['http://127.0.0.1:7777', 
			   'http://127.0.0.1:7778', 
			   'http://127.0.0.1:7779'];
	    var count = {};
	    servers.forEach(function(server) {
		hash.addServer(server);
		count[server] = 0;
	    });
	    for(var i = 0; i < 10; i++) {
		let server = hash.getServerFor('foo' + i);
		count[server] += 1;
	    }
	    servers.forEach(function(server) {
		assert(count[server] > 0, count[server] + ' > 0');
	    });
	});
    });
    describe('.removeServer(server)', function(){
	it('should make sure a server is no longer available for selection', function(){
	    var hash = new NodeHash(100);
	    var servers = ['http://127.0.0.1:7777', 
			   'http://127.0.0.1:7778', 
			   'http://127.0.0.1:7779'];
	    var count = {};
	    servers.forEach(function(server) {
		hash.addServer(server);
		count[server] = 0;
	    });
	    // Remove...
	    hash.removeServer('http://127.0.0.1:7778');
	    for(var i = 0; i < 10; i++) {
		let server = hash.getServerFor('foo' + i);
		count[server] += 1;
	    }
	    assert.equal(count['http://127.0.0.1:7778'], 0);
	});

    });

});
