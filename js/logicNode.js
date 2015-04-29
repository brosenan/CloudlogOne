"use strict";
var bodyParser = require('body-parser')
var PeerLocator = require('./peerLocator.js');
var ChunkStore = require('./chunkStore.js');
var PrologInterface = require('./prologInterface.js');
var $S = require('suspend'), $R = $S.resume;
var assert = require('assert');
var request = require('request');

function UpstreamClient(node, results) {
    this.apply = $S.async(function*(v, patch) {
	var split = v.split(',');
	var peer;
	if(node._locator.knownPeers().length > 0) {
	    peer = node._locator.getServerFor(split[0]);
	} else {
	    peer = 'http://localhost:' + node._port;
	}
	var postOpts = {
	    method: 'POST',
	    json: true,
	};
	if(split[1] === "'_'") {
	    postOpts.uri = peer + '/new';
	    postOpts.body = {id: split[0], patch: patch};
	} else {
	    postOpts.uri = peer + '/apply';
	    postOpts.body = {ver: v, patch: patch};
	}
	var resp = yield request(postOpts, $S.resumeRaw());
	assert.ifError(resp[0]);
	assert.equal(resp[1].statusCode, 200);
	if(resp[2].res) {
	    resp[2].res.forEach(function(r) {
		results.push(r);
	    });
	}
	return resp[2].ver;
    });
}

module.exports = function(options, bucketStore) {
    var self = this;
    this._port = options.port;
    this._locator = new PeerLocator(this._port, options.peer, options.clusterSize);
    this._prolog = new PrologInterface('/tmp/logicNode.' + options.port + '.log');
    if(options.maxDepth) {
	this._prolog.request('set_max_depth(' + options.maxDepth + ')');
    }
    this._chunkStore = new ChunkStore(this._prolog, null, bucketStore, options);
    this._app = this._locator.app();
    this._app.post('/new', function(req, res) {
	$S.run(function*() {
	    var id = req.body.id;
	    var chunk = yield self._chunkStore.getChunk(id, $R());
	    var ver = yield chunk.init(req.body.patch, $R());
	    res.json({ver: ver});
	});
    });
    this._app.post('/apply', function(req, res) {
	$S.run(function*() {
	    var ver = req.body.ver;
	    var chunk = yield self._chunkStore.getChunk(ver, $R());
	    var results = [];
	    chunk.setUpstream(new UpstreamClient(self, results));
	    ver = yield chunk.apply(ver, 
				       req.body.patch, 
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