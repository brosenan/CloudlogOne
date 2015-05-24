"use strict";
var assert = require('assert');
var EventEmitter = require('events').EventEmitter;
var $S = require('suspend'), $R = $S.resume, $T = function(gen) { return function(done) { $S.run(gen, done); } };
var sinon = require('sinon');

var PrologInterface = require('../js/prologInterface.js');
var Chunk = require('../js/chunk.js');

var prolog = null;
var upstream = {};
var vercast = require('vercast');

function bucketStore() {
	var bs = new vercast.DummyBucketStore();
	return bs;
}

describe('Chunk', function(){
	beforeEach(function() {
		// prolog = new PrologInterface('chunk-test.log');
		prolog = new PrologInterface();
	});
	describe('.init(patch, cb(err, v0))', function(){
		it('should initialize a new chunk based on the given creation patch', $T(function*(){
			var chunk = new Chunk(prolog, null, bucketStore());
			var initResp = yield chunk.init('add_v((foo(bar) :- true), 1)', $R());
			var v0 = initResp.ver;
			assert.equal(typeof v0, 'string');
			var split = v0.split(',');
			assert.equal(split[0], split[1]);
			// init() should also return a client-patch
			assert.equal(typeof initResp.clientPatches[0], 'string');
			assert.equal(initResp.clientPatches[0], 'h_putPlaceholder((foo(bar):-true), (' + v0 + '))');
		}));
		it('should report errors to the callback', $T(function*(){
			var chunk = new Chunk(prolog, null, bucketStore());
			try {
				var v0 = yield chunk.init('foobar', $R());
				assert(false, 'Should fail');
			} catch(e) {
				assert.equal(e.message, 'unknownCommand(create(foobar))');
			}
		}));
	});
	describe('.apply(v1, patch, cb(err, {ver,clientPatches,results}))', function(){
		it('should call the callback with the new version ID', $T(function*(){
			var chunk = new Chunk(prolog, null, bucketStore());
			var v = (yield chunk.init('add_v((foo(bar) :- true), 1)', $R())).ver;
			var v2 = (yield chunk.apply(v, 'logicQuery(X, foo(X), 1)', $R())).ver;
			assert.equal(typeof v2, 'string');
		}));
		it('should emit downstream results', $T(function*(){
			var chunk = new Chunk(prolog, null, bucketStore());
			var v = (yield chunk.init('add_v((foo(bar):-true), 1)', $R())).ver;
			var res = (yield chunk.apply(v, 'logicQuery(X, foo(X), 1)', $R())).results;
			assert.deepEqual(res, ['res(bar,1)']);
		}));
		it('should return a list of client patches to perform', $T(function*() {
			var chunk = new Chunk(prolog, null, bucketStore());
			var v = (yield chunk.init('add_v((foo(bar):-true), 1)', $R())).ver;
			var res = yield chunk.apply(v, 'logicQuery(X, foo(X), 1)', $R());
			assert.deepEqual(res.clientPatches, []);
			res = yield chunk.apply(res.ver, 'add_v((foo(baz):-true), 1)', $R());
			assert.equal(res.clientPatches[0], 'h_updatePlaceholder((foo(baz):-true), (' + v + '), (' + res.ver + '))');
		}));
		it('should forward any upstream requests to the upstream client', $T(function*(){
			var upstream = {};
			var chunk = new Chunk(prolog, upstream, bucketStore());
			yield prolog.request('set_max_depth(1)').on('done', $R());
			var v = (yield chunk.init('add_v((foo(bar):-true), 1)', $R())).ver;
			upstream.apply = sinon.spy($S.resumeRaw());
			var em = new EventEmitter();
			chunk.apply(v, 'add_v((bar(foo):-true), 1)', em2cb(em));
			yield; // Wait until the spy is called
			// The upstream apply() method must be called, requiring a new chunk to be created
			assert(upstream.apply.calledOnce, 'upstream.apply.calledOnce');
			// The first argument is the ID, with '_' as the second part, indicating a new chunk.
			assert.equal(upstream.apply.firstCall.args[0].split(',')[1], "'_'");
			// The second argument is the initialization patch.
			assert(upstream.apply.firstCall.args[1].match(/bar\(foo\):-true/),
				   upstream.apply.firstCall.args[1] + " match bar(foo):-true");
			// The third argument should be a callback, that receives the client-patch.
			var firstHalf = upstream.apply.firstCall.args[0].split(',')[0];
			upstream.apply.firstCall.args[2](undefined, 'h_putPlaceholder((bar(foo):-true),(' + firstHalf + ',' + firstHalf + '))');
			var res = (yield em.on('success', $S.resumeRaw()))[0];
			var newID = res.ver;
			assert.equal(res.clientPatches.length, 1);
			// Further operations should be forwarded to the new chunk
			upstream.apply = sinon.spy($S.resumeRaw());
			em = new EventEmitter();
			chunk.apply(newID, 'add_v((bar(boo):-true),1)', em2cb(em));
			yield;
			// The first argument should be the ID of the new chunk
			assert.equal(upstream.apply.firstCall.args[0], firstHalf + ',' + firstHalf);
			// The second argument should be the patch to be forwarded
			assert.equal(upstream.apply.firstCall.args[1], '[add_v((bar(boo):-true),1)]');
			// The third argument is a callback.  Calling it will replace the placeholder
			upstream.apply.firstCall.args[2](undefined, 'h_updatePlaceholder((bar(foo):-true),(' + firstHalf + ',' + firstHalf + '),(' + firstHalf + ',xxx' + '))');
			newID = (yield em.on('success', $S.resumeRaw()))[0].ver;
			// Now the placeholder should be ...,xxx
			upstream.apply = sinon.spy($S.resumeRaw());
			em = new EventEmitter();
			chunk.apply(newID, 'add_v((bar(boo):-true),1)', em2cb(em));
			yield;
			assert.equal(upstream.apply.firstCall.args[0], firstHalf + ',xxx');
		}));
		it.skip('should replace placeholders only when they change', $T(function*(){
			var upstream = {};
			var chunk = new Chunk(prolog, upstream, bucketStore());
			yield prolog.request('set_max_depth(1)').on('done', $R());
			var v = yield chunk.init('add_v((foo(bar):-true), 1)', $R());
			// Create a placeholder for a(X)
			upstream.apply = $S.resumeRaw();
			var em = new EventEmitter();
			chunk.apply(v, 'add_v(a(1), 1)', function() {}, em2cb(em));
			var args = yield;
			// Fake a response from the upstream client
			var upV = args[0].replace("'_'", 'foo');
			args[2](undefined, upV);
			v = (yield em.on('success', $S.resumeRaw()))[0];
			// Now we have a placeholder '...',foo
			upstream.apply = $S.resumeRaw();
			em = new EventEmitter();
			chunk.apply(v, 'add_m(rule(a(X), true, b(X)), 1)', function() {}, em2cb(em));
			args = yield;
			args[2](undefined, args[0].replace('foo', 'bar'));
			yield em.on('success', $S.resumeRaw());
		}));
	});
	describe('.open(id, cb(err))', function(){
		it('should restore the content of a chunk with the same ID', $T(function*(){
			var bs = bucketStore();
			bs.delay = 1;
			var chunk1 = new Chunk(prolog, null, bs);
			var v = (yield chunk1.init('add_v((foo(bar):-true), 1)', $R())).ver;
			var chunk2 = new Chunk(new PrologInterface(), null, bs);
			yield chunk2.open(v.split(',')[0], $R());
			var res = yield chunk2.apply(v, 'logicQuery(X, foo(X), 1)', $R());
			assert.deepEqual(res.results, ['res(bar,1)']);
		}));
		it('should restore patches applied to chunks', $T(function*(){
			var bs = bucketStore();
			bs.delay = 1;
			var chunk1 = new Chunk(prolog, null, bs);
			var v = (yield chunk1.init('add_v((foo(bar):-true), 1)', $R())).ver;
			v = (yield chunk1.apply(v, 'add_v((bar(foo):-true), 1)', $R())).ver;
			var chunk2 = new Chunk(new PrologInterface(), null, bs);
			yield chunk2.open(v.split(',')[0], $R());
			var res = yield chunk2.apply(v, 'logicQuery(X, bar(X), 1)', $R());
			assert.deepEqual(res.results, ['res(foo,1)']);
		}));
	});
});


function em2cb(em) {
	return function(err, data) {
		if(err) {
			em.emit('error', err);
		} else {
			em.emit('success', data);
		}
	}
}
