"use strict";
var assert = require('assert');
var $S = require('suspend'), $R = $S.resume, $T = function(gen) { return function(done) { $S.run(gen, done); } };

var PrologInterface = require('../js/prologInterface.js');

describe('PrologInterface', function(){
    it('should return an event emitter', function(done){
	var prolog = new PrologInterface();
	var emitter = prolog.request("heartbeat");
	emitter.on('done', done);
    });
    it('should emit an error on unknown events', function(done){
	var prolog = new PrologInterface();
	var emitter = prolog.request("foo(bar)");
	emitter.on('error', function(err) {
	    done();
	});
    });
    it('should emit a success event with a result on success', function(done){
	var prolog = new PrologInterface();
	var emitter = prolog.request("heartbeat");
	emitter.on('success', function(status) {
	    assert.equal(status, "alive");
	    done();
	});
    });
    it('should emit done even in case of an error', function(done){
	var prolog = new PrologInterface();
	var emitter = prolog.request("foo(bar)");
	emitter.on('error', function(err) {});
	emitter.on('done', done);
    });
    it('should emit events only for the current request', $T(function*(){
	var prolog = new PrologInterface();
	var emitter1 = prolog.request("heartbeat");
	var count = 0;
	emitter1.on('success', function() {
	    count += 1;
	});
	yield emitter1.on('done', $R());
	var emitter2 = prolog.request("heartbeat");
	yield emitter2.on('done', $R());
	assert.equal(count, 1);
    }));
    it('should queue requests so that they do not overlap', $T(function*(){
	var prolog = new PrologInterface();
	var emitter1 = prolog.request("heartbeat");
	var count = 0;
	emitter1.on('success', function() {
	    count += 1;
	});
	// not waiting
	var emitter2 = prolog.request("heartbeat");
	yield emitter2.on('done', $R());
	assert.equal(count, 1);
    }));

});
