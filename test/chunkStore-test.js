"use strict";
var assert = require('assert');
var $S = require('suspend'), $R = $S.resume, $T = function(gen) { return function(done) { $S.run(gen, done); } };

var PrologInterface = require('../js/prologInterface.js');
var ChunkStore = require('../js/chunkStore.js');
var Chunk = require('../js/chunk.js');

var prolog;
var vercast = require('vercast');

function bucketStore() {
	var bs = new vercast.DummyBucketStore();
	return bs;
}

describe('ChunkStore', function(){
	beforeEach(function() {
//		prolog = new PrologInterface('foo.log');
		prolog = new PrologInterface();
	});
	
	describe('.getChunk(v, cb(err, chunk))', function(){
		it('should return a chunk for the given version', $T(function*(){
			var chunkStore = new ChunkStore(prolog, null, bucketStore());
			var c1 = yield chunkStore.getChunk('xxx,aaa', $R());
			assert(c1 instanceof Chunk, c1 + ' instanceof Chunk');
			var c2 = yield chunkStore.getChunk('xxx,bbb', $R());
			assert.equal(c1, c2);
			var c3 = yield chunkStore.getChunk('yyy,ccc', $R());
			assert.notEqual(c3, c1);
		}));
		it('should prune chunks that fall out of cache', $T(function*(){
			var chunkStore = new ChunkStore(prolog, null, bucketStore(), {capacity: 1});
			var c1 = yield chunkStore.getChunk("'t97fVauK9CLRD0GP5GEiFsUxupo=',aaa", $R());
			var id = yield c1.init('add_v((a(1):-true), 1)', $R());
			// Fetching c2 should force c1 out of the cache
			var c2 = yield chunkStore.getChunk("'rlvjsFVoCtB+OyTTF7zld3YCUII=',bbb", $R());
			yield c2.init('add_v((b(2):-true), 1)', $R());
			yield setTimeout($R(), 100); // Waiting to allow the pruning take place
			var err = yield prolog.request('on((' + id + '), logicQuery(X, a(X), 1))').on('error', $S.resumeRaw());
			assert.equal(err[0].message, "error(chunkDoesNotExist('t97fVauK9CLRD0GP5GEiFsUxupo='))");
			// Requesting c1 again should restore its state
			c1 = yield chunkStore.getChunk("'t97fVauK9CLRD0GP5GEiFsUxupo=',aaa", $R());
			var res = yield c1.apply(id, 'logicQuery(X, a(X), 1)', $S.resumeRaw(), function() {});
			assert.equal(res[0], 'res(1,1)');
		}));

	});

});
