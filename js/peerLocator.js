"use strict";
var express = require('express');
var bodyParser = require('body-parser')
var request = require('request');
var $S = require('suspend'), $R = $S.resume;

var SUFFIX = '/_peerloc';

module.exports = function(port, master) {
    var self = this;
    this._app = express();
    this._port = port;
    this._master = master;
    this._peers = {};
    if(master) {
	this._peers[master] = 1;
    }
    this._app.use(bodyParser.json());
    this._app.post(SUFFIX, function(req, res) {
	self._addPeerAddress(req);
	self._mergeResponse(req.body);
	res.send(self._myPeers());
    });
};
var clazz = module.exports.prototype;

clazz.app = function() {
    return this._app;
};

clazz.run = $S.async(function*() {
    yield this._app.listen(this._port, $R());
    if(this._master) {
	let masterResp = yield request({
	    uri: this._master + SUFFIX, 
	    json: true,
	    body: this._myPeers(),
	    method: 'POST',
	}, $S.resumeRaw());
	if(masterResp[0]) throw masterResp[0];
	this._mergeResponse(masterResp[2]);
    }
});

//clazz.getServerFor = function(id) {
//    return Object.keys(this._peers)[0];
//};

clazz.knownPeers = function() {
    return Object.keys(this._peers);
};

clazz._mergeResponse = function(resp) {
    var self = this;
    Object.keys(resp.peers).forEach(function(peer) {
	self._addPeer(peer);
    });
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