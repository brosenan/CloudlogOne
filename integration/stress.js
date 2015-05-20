"use strict";
var assert = require('assert');
var request = require('request');
var LogicNode = require('../js/logicNode.js');
var $S = require('suspend'), $R = $S.resume;
var vercast = require('vercast');

var NUM_NODES = 5;
var BASE_PORT = 12000;
var NUM_I = 100;
var NUM_J = 100;
var MAX_DEPTH = 7;

var bucketStore = new vercast.DummyBucketStore();

var setUp = $S.async(function*() {
	console.log('setup');
	var nodes = [];
	for(let i = 0; i < NUM_NODES; i++) {
		let opts = {
			port: BASE_PORT + i,
			peer: 'http://localhost:' + BASE_PORT,
			maxDepth: MAX_DEPTH,
			clusterSize: NUM_NODES * 3,
		};
		let node = new LogicNode(opts, bucketStore);
		nodes.push(node);
		yield node.start($R());
	}
	console.log('setup - done');
	return nodes;
});

var send = $S.async(function*(nodeNum, message) {
	var opts = {
		uri: 'http://localhost:' + (BASE_PORT + nodeNum) + '/',
		method: 'POST',
		json: true,
		body: message,
	};
	var resp = yield request(opts, $S.resumeRaw());
	assert.ifError(resp[0]);
	assert.equal(resp[1].statusCode, 200, resp[2]);
	return resp[2];
});

var populate = $S.async(function*() {
	var res = yield send(0, {patches: []}, $R());
	for(let i = 0; i < NUM_I; i++) {
		console.log(i);
		for(let j = 0; j < NUM_J; j++) {
			res = yield send(j % NUM_NODES, {ver: res.ver,
										   patches: ['add((mult(' + j + ',' + i + ',' + (i*j) + '):-true),1)']}, $R());
		}
	}
	return res.ver;
});
var query = $S.async(function*(ver) {
	for(let i = 0; i < NUM_I; i++) {
		console.log(i);
		for(let j = 0; j < NUM_J; j++) {
			let res = yield send(j % NUM_NODES, {ver: ver,
											 patches: ['logicQuery(X, mult(' + j + ',' + i + ',X), 1)']}, $R());
			assert.equal(res.results.length, 1);
			assert.equal(res.results[0], 'res(' + (i*j) + ',1)');
		}
	}
});
var tearDown = $S.async(function*(nodes) {
	console.log('tearDown');
	nodes.forEach(function(node) {
		node.stop($S.fork());
	});
	yield $S.join();
	console.log('tearDown - done');
});

$S.run(function*() {
	var nodes = yield setUp($R());
	try {
		var ver = yield populate($R());
		yield query(ver, $R());
	} finally {
		yield tearDown(nodes, $R());
	}
});
