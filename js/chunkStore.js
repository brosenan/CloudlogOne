"use strict";
var Chunk = require('./chunk.js');
var LRU = require('lru').LRU;
var $S = require('suspend'), $R = $S.resume;

module.exports = function(prolog, upstream, bucketStore, options) {
	options = options || {};
	var capacity = options.capacity || 1000;
	this._prolog = prolog;
	this._upstream = upstream;
	this._bucketStore = bucketStore;
	this._cache = new LRU(capacity);
	this._cache.on('evict', function(args) {
		prolog.request('prune(' + args.key + ')');
	});
};
var clazz = module.exports.prototype;

clazz.getChunk = $S.async(function*(v) {
	var id = v.split(',')[0];
	var chunk = this._cache.get(id);
	if(!chunk) {
		chunk = new Chunk(this._prolog, this._upstream, this._bucketStore);
		this._cache.set(id, chunk);
		yield chunk.open(id, $R());
	}
	return chunk;
});
