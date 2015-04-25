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
	    //assert.equal(l2.knownPeers().length, 3);
	    assert.equal(l3.knownPeers().length, 3);
	}));

    });

});
