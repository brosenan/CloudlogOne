"use strict";
var assert = require('assert');
var http = require('http');
var request = require('request');

var $S = require('suspend'), $R = $S.resume, $T = function(gen) { return function(done) { $S.run(gen, done); } };

var PeerLocator = require('../js/peerLocator.js');

describe('PeerLocator', function(){
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
});
