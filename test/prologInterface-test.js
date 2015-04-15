"use strict";
var assert = require('assert');
var $S = require('suspend'), $R = $S.resume, $T = function(gen) { return function(done) { $S.run(gen, done); } };

var PrologInterface = require('../js/prologInterface.js');

describe('PrologInterface', function(){
    describe('.request(req)', function(){
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
    it('should support creation of new chunks', $T(function*(){
	var prolog = new PrologInterface();
	var em = prolog.request("create([add_v((foo(bar) :- true), 1)])");
	var r = $R();
	var id = (yield em.on('success', $S.resumeRaw()))[0];
	var split = id.split(',');
	assert.equal(split[0], split[1]);
    }));
    function* createChunk(prolog, ops) {
	var em = prolog.request("create([" + ops.join(",") + "])");
	var r = $R();
	return (yield em.on('success', $S.resumeRaw()))[0];
    }
    it('should support queries on chunks', $T(function*(){
	var prolog = new PrologInterface();
	var id = yield* createChunk(prolog, ["add_v((foo(bar) :- true), 1)"]);
	var em = prolog.request("on((" + id + "), logicQuery(X, foo(X), 1))");
	var res = (yield em.on('downstream', $S.resumeRaw()))[0];
	assert.equal(res, "res(bar,1)");
    }));
    it('should support patches', $T(function*(){
	var prolog = new PrologInterface();
	var id = yield* createChunk(prolog, []);
	var em = prolog.request("on((" + id + "), add_v((foo(bar) :- true), 1))");
	id = (yield em.on("success", $S.resumeRaw()))[0];
	em = prolog.request("on((" + id + "), logicQuery(X, foo(X), 1))");
	var res = (yield em.on('downstream', $S.resumeRaw()))[0];
	yield em.on('done', $R());
	assert.equal(res, "res(bar,1)");
    }));
    it('should forward requests to other chunks when encountering a placeholder', $T(function*(){
	var prolog = new PrologInterface();
	var id = yield* createChunk(prolog, ["add_v(a(b), 1)", "h_putPlaceholder(x(y), (foo,bar))"]);
	// Query
	var em = prolog.request("on((" + id + "), logicQuery(X, foo(X), 1))");
	em.on("downstream", function(data) {
	    assert(false, 'This query should not have downstream results: ' + data);
	});
	var forward = yield em.on("upstream", $S.resumeRaw());
	assert.equal(forward[0], "foo,bar");
	assert(forward[1].match(/logicQuery\(_.*,foo\(_.*\),1\)/), "valid forward query: " + forward[1]);

	// add_v
	em = prolog.request("on((" + id + "), add_v(a(c), 1))");
	forward = yield em.on("upstream", $S.resumeRaw());
	assert.deepEqual(forward, ['foo,bar', 'add_v(a(c),1)']);

	// add_m
	em = prolog.request("on((" + id + "), add_m(rule(a(X), true, b(X)), 1))");
	forward = yield em.on("upstream", $S.resumeRaw());
    }));

    it('should request the creation of a new chunk if the size limit has been exceeded', $T(function*(){
	var prolog = new PrologInterface();
	var id = yield* createChunk(prolog, []);
	var count = 0;
	for(let i = 0; i < 2000; i++) {
	    let em = prolog.request("on((" + id + "), add_v(a(" + i + "), 1))");
	    em.on("upstream", function(fid, req) {
		assert.equal(fid, "'_','_'");
		count += 1;
	    });
	    id = (yield em.on("success", $S.resumeRaw()))[0];
	}
	assert(count > 0, 'count > 0');
    }));
});
