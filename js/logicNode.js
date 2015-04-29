"use strict";
var bodyParser = require('body-parser')
var PeerLocator = require('./peerLocator.js');
var ChunkStore = require('./chunkStore.js');
var PrologInterface = require('./prologInterface.js');
var $S = require('suspend'), $R = $S.resume;

module.exports = function(options, bucketStore) {
    var self = this;
    this._locator = new PeerLocator(options.port, options.peer, options.clusterSize);
    this._chunkStore = new ChunkStore(new PrologInterface('/tmp/logicNode.log'), null, bucketStore, options);
    this._app = this._locator.app();
    this._app.post('/new', function(req, res) {
	$S.run(function*() {
	    var id = req.body.id;
	    var chunk = yield self._chunkStore.getChunk(id, $R());
	    var ver = yield chunk.init('[' + req.body.patches.join(',') + ']', $R());
	    res.json({ver: ver});
	});
    });
    this._app.post('/apply', function(req, res) {
	$S.run(function*() {
	    var ver = req.body.ver;
	    var chunk = yield self._chunkStore.getChunk(ver, $R());
	    var results = [];
	    ver = yield chunk.apply(ver, 
				       '[' + req.body.patches.join(',') + ']', 
				       function(r) { results.push(r); },
				       $R());
	    res.json({ver: ver, res: results});
	});
    });
};

var clazz = module.exports.prototype;

clazz.start = function(cb) { 
    return this._locator.run(cb); 
};

clazz.stop = function(cb) {
    this._locator.stop(cb);
};