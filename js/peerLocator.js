"use strict";
var express = require('express');
var bodyParser = require('body-parser')
var request = require('request');
var $S = require('suspend'), $R = $S.resume;
var NodeHash = require('./nodehash.js');

var SUFFIX = '/_peerloc';

module.exports = function(port, master, size) {
    var self = this;
    size = size || 100;
    this._nodehash = new NodeHash(size);
    this._app = express();
    this._port = port;
    this._master = master;
    this._peers = {};
    if(master) {
	this._peers[master] = 1;
	this._nodehash.addServer(master);
    }
    this._app.use(bodyParser.json());
    this._app.post(SUFFIX, function(req, res) {
	self._addPeerAddress(req);
	self._mergeResponse(req.body);
	res.send(self._myPeers());
    });
    this._server = null;
};
var clazz = module.exports.prototype;

clazz.app = function() {
    return this._app;
};

clazz.run = $S.async(function*() {
    this._server = this._app.listen(this._port, $R());
    yield; // Wait for the server to come up
    if(this._master) {
	yield this._contactPeer(this._master, $R());
    }
});

clazz.getServerFor = function(key) {
    return this._nodehash.getServerFor(key);
};

clazz.knownPeers = function() {
    return Object.keys(this._peers);
};

clazz._mergeResponse = function(resp) {
    var self = this;
    var unknown = [];
    Object.keys(resp.peers).forEach(function(peer) {
	if(!(peer in self._peers)) {
	    self._nodehash.addServer(peer);
	    self._addPeer(peer);
	    unknown.push(peer);
	}
    });
    return unknown;
};

clazz._addPeer = function(peer) {
    this._peers[peer] = 1;
};

clazz._myPeers = function() {
    return {
	myPort: this._port,
	peers: this._peers,
    };
};

clazz._addPeerAddress = function(req) {
    var ip = req.ip.match(/[^:]*$/)[0];
    var url = 'http://' + ip + ':' + req.body.myPort;
    req.body.peers[url] = 1;
}

clazz._contactPeer = $S.async(function*(peer) {
    var self = this;
    var resp = yield request({
	uri: peer + SUFFIX, 
	json: true,
	body: this._myPeers(),
	method: 'POST',
    }, $S.resumeRaw());
    if(resp[0]) {
	throw resp[0];
    }
    var unknown = this._mergeResponse(resp[2]);
    unknown.forEach(function(other) {
	self._contactPeer(other, $S.fork());
    });
    yield $S.join();
});

clazz.stop = $S.async(function*() {
    yield this._server.close($R());
});