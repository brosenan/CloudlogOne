"use strict";
var assert = require('assert');
var EventEmitter = require('events').EventEmitter;
var $S = require('suspend'), $R = $S.resume, $T = function(gen) { return function(done) { $S.run(gen, done); } };
var sinon = require('sinon');

var PrologInterface = require('../js/prologInterface.js');
var Chunk = require('../js/chunk.js');

var prolog = null;
var upstream = {};

function bucketStore() {
}

describe('Chunk', function(){
    beforeEach(function() {
	prolog = new PrologInterface('chunk-test.log');
//	prolog = new PrologInterface();
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
	it('should forward any upstream requests to the upstream client', $T(function*(){
	    var upstream = {};
	    var chunk = new Chunk(prolog, upstream);
	    yield prolog.request('set_max_depth(1)').on('done', $R());
	    var v = yield chunk.init('add_v((foo(bar):-true), 1)', $R());
	    upstream.apply = sinon.spy($S.resumeRaw());
	    var em = chunk.apply(v, 'add_v((bar(foo):-true), 1)');
	    yield; // Wait until the spy is called
	    // The upstream apply() method must be called, requiring a new chunk to be created
	    assert(upstream.apply.calledOnce, 'upstream.apply.calledOnce');
	    // The first argument is the ID, with '_' as the second part, indicating a new chunk.
	    assert.equal(upstream.apply.firstCall.args[0].split(',')[1], "'_'");
	    // The second argument is the initialization patch.
	    assert(upstream.apply.firstCall.args[1].match(/bar\(foo\):-true/), 
		   upstream.apply.firstCall.args[1] + " match bar(foo):-true");
	    // The third argument should be a callback, that receives the new ID.
	    var firstHalf = upstream.apply.firstCall.args[0].split(',')[0];
	    upstream.apply.firstCall.args[2](undefined, firstHalf + ',' + firstHalf);
	    var newID = (yield em.on('success', $S.resumeRaw()))[0];
	    // Further operations should be forwarded to the new chunk
	    upstream.apply = sinon.spy($S.resumeRaw());
	    em = chunk.apply(newID, 'add_v((bar(boo):-true),1)');
	    yield;
	    // The first argument should be the ID of the new chunk
	    assert.equal(upstream.apply.firstCall.args[0], firstHalf + ',' + firstHalf);
	    // The second argument should be the patch to be forwarded
	    assert.equal(upstream.apply.firstCall.args[1], 'add_v((bar(boo):-true),1)');
	    // The third argument is a callback.  Calling it will replace the placeholder
	    upstream.apply.firstCall.args[2](undefined, firstHalf + ',xxx');
	    newID = (yield em.on('success', $S.resumeRaw()))[0];
	    // Now the placeholder should be ...,xxx
	    upstream.apply = sinon.spy($S.resumeRaw());
	    em = chunk.apply(newID, 'add_v((bar(boo):-true),1)');
	    yield;
	    assert.equal(upstream.apply.firstCall.args[0], firstHalf + ',xxx');
	}));
	it('should replace placeholders only when they change', $T(function*(){
	    var upstream = {};
	    var chunk = new Chunk(prolog, upstream);
	    yield prolog.request('set_max_depth(1)').on('done', $R());
	    var v = yield chunk.init('add_v((foo(bar):-true), 1)', $R());
	    // Create a placeholder for a(X)
	    upstream.apply = $S.resumeRaw();
	    var em = chunk.apply(v, 'add_v(a(1), 1)');
	    var args = yield;
	    // Fake a response from the upstream client
	    var upV = args[0].replace("'_'", 'foo');
	    args[2](undefined, upV);
	    v = (yield em.on('success', $S.resumeRaw()))[0];
	    // Now we have a placeholder '...',foo
	    upstream.apply = $S.resumeRaw();
	    em = chunk.apply(v, 'add_m(rule(a(X), true, b(X)), 1)');
	    args = yield;
	    args[2](undefined, args[0].replace('foo', 'bar'));
	    yield em.on('success', $S.resumeRaw());
	}));
	it('should emit downstream results', $T(function*(){
	    var chunk = new Chunk(prolog);
	    var v = yield chunk.init('add_v((foo(bar):-true), 1)', $R());
	    var em = chunk.apply(v, 'logicQuery(X, foo(X), 1)');
	    var res = yield em.on('downstream', $S.resumeRaw());
	    assert.deepEqual(res, ['res(bar,1)']);
	}));

    });
    describe('.open(id, cb(err))', function(){
	it('should restore the content of a chunk with the same ID', $T(function*(){
	    var bs = bucketStore();
	    var chunk1 = new Chunk(prolog, null, bs);
	    var v = yield chunk1.init('add_v((foo(bar):-true), 1)', $R());
	    var chunk2 = new Chunk(new PrologInterface(), null, bs);
	    var em = chunk2.apply(v, 'logicQuery(X, foo(X), 1)');
	    var res = yield em.on('downstream', $S.resumeRaw());
	    assert.deepEqual(res, ['res(bar,1)']);
	}));
    });
});
