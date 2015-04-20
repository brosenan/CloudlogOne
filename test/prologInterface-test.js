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
    describe('create/1', function(){
	it('should support creation of new chunks', $T(function*(){
	    var prolog = new PrologInterface();
	    var em = prolog.request("create([add_v((foo(bar) :- true), 1)])");
	    var r = $R();
	    var id = (yield em.on('success', $S.resumeRaw()))[0];
	    var split = id.split(',');
	    assert.equal(split[0], split[1]);
	}));
	
    });
    function* createChunk(prolog, ops) {
	var em = prolog.request("create([" + ops.join(",") + "])");
	var r = $R();
	return (yield em.on('success', $S.resumeRaw()))[0];
    }
    describe('on/2', function(){
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
	    yield setMaxDepth(prolog, 1, $S.resumeRaw());
	    
	    var id = yield* createChunk(prolog, ['add_v(a, 1)']);
	    
	    var em = prolog.request("on((" + id + "), add_m(rule(b, true, c), 1))");
	    id = (yield em.on("success", $S.resumeRaw()))[0];
	    
	    em = prolog.request("on((" + id + "), add_v(b, 1))");

	    var res = yield em.on("upstream", $S.resumeRaw());
	    var split = res[0].split(',');
	    assert.equal(split[1], "'_'");
	    assert(res[1].match(/add_m\(rule\(b,true,c\),1\)/), res[1] + ' match add_m(rule(b,true,c),1)');
	    assert(res[1].match(/add_v\(b,1\)/), res[1] + ' match add_v(b,1)');

	    // The first part of the ID should be the ID of the new chunk
	    prolog = new PrologInterface();
	    var newID = (yield prolog.request('create(' + res[1] + ')').on('success', $S.resumeRaw()))[0];
	    assert.equal(split[0], newID.split(',')[0]);
	}));
	
	it('should convert pending hooks to creation patches', $T(function*(){
	    var prolog = new PrologInterface();
	    yield setMaxDepth(prolog, 1, $S.resumeRaw());
	    var id = yield* createChunk(prolog, ['add_v(foo, 1)']);
	    id = (yield prolog.request("on((" + id + "), add_m(rule(a(X), true, b(X)), 1))").on("success", $S.resumeRaw()))[0];
	    id = (yield prolog.request("on((" + id + "), add_m(rule(b(X), true, c(X)), 1))").on("success", $S.resumeRaw()))[0];
	    var fwd = (yield prolog.request("on((" + id + "), add_v(a(b), 1))").on("upstream", $S.resumeRaw()));
	    assert(fwd[1].match(/add_m\(rule\(a\(_G[0-9]*\),true,b\(_G[0-9]*\)\),1\)/), 'should match add_m(rule(a(X), true, b(X)), 1)): ' + fwd[1]);
	    assert(fwd[1].match(/add_m\(rule\(b\(_G[0-9]*\),true,c\(_G[0-9]*\)\),1\)/), 'should match add_m(rule(b(X), true, c(X)), 1)): ' + fwd[1]);
	}));

    });
    function setMaxDepth(prolog, depth, cb) {
	var em = prolog.request('set_max_depth(' + depth + ')');
	em.on("success", cb);
    }
    describe('set_max_depth/1', function(){
	it('should set the maximum depth of trees', $T(function*(){
	    var prolog = new PrologInterface();
	    var em = prolog.request('set_max_depth(1)');
	    var res = yield em.on("success", $S.resumeRaw());
	    assert.equal(res[0], 'was: 20');

	    var id = yield* createChunk(prolog, ['add_v(foo, 1)']);
	    em = prolog.request('on((' + id + '), add_v(a(1), 1))');
	    // With depth 0, any addition should go upstream
	    res = yield em.on('upstream', $S.resumeRaw());
	}));
    });
});
