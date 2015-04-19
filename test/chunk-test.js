"use strict";
var assert = require('assert');
var $S = require('suspend'), $R = $S.resume, $T = function(gen) { return function(done) { $S.run(gen, done); } };

var PrologInterface = require('../js/prologInterface.js');
var Chunk = require('../js/chunk.js');

var prolog = new PrologInterface();

describe('Chunk', function(){
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

});
