"use strict";
var assert = require('assert');
var EventEmitter = require('events').EventEmitter;
var $S = require('suspend'), $R = $S.resume, $T = function(gen) { return function(done) { $S.run(gen, done); } };

var PrologInterface = require('../js/prologInterface.js');
var Chunk = require('../js/chunk.js');

var prolog = null;

describe('Chunk', function(){
    beforeEach(function() {
	prolog = new PrologInterface(/*'chunk-test.log'*/);
    });
    describe('.init(patch, cb(err, v0))', function(){
	it('should initialize a new chunk based on the given creation patch', $T(function*(){
	    var chunk = new Chunk(prolog);
	    var v0 = yield chunk.init('add_v((foo(bar) :- true), 1)', $R());
	    assert.equal(typeof v0, 'string');
	    var split = v0.split(',');
	    assert.equal(split[0], split[1]);
	}));
	it('should report errors to the callback', $T(function*(){
	    var chunk = new Chunk(prolog);
	    try {
		var v0 = yield chunk.init('foobar', $R());
		assert(false, 'Should fail');
	    } catch(e) {
		assert.equal(e.message, 'unknownCommand(create(foobar))');
	    }
	}));
    });
    describe('.apply(v1, patch)', function(){
	it('should return an EventEmitter', $T(function*(){
	    var chunk = new Chunk(prolog);
	    var v = yield chunk.init('add_v((foo(bar) :- true), 1)', $R());
	    var em = chunk.apply(v, 'logicQuery(X, foo(X), 1)');
	    assert(em instanceof EventEmitter, em + ' instanceof EventEmitter');
	}));
	it('should emit a success event, providing the new version', $T(function*(){
	    var chunk = new Chunk(prolog);
	    var v = yield chunk.init('add_v((foo(bar) :- true), 1)', $R());
	    var em = chunk.apply(v, 'logicQuery(X, foo(X), 1)');
	    var v2 = yield em.on('success', $S.resumeRaw());
	}));

    });
});
