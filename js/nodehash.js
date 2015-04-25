"use strict";
var crypto = require('crypto');

module.exports = function(size, mult) {
    this.mult = mult || 3;
    this.size = size;
    this.array = Array(size);
    for(var i = 0; i < size; i++) {
	this.array[i] = [];
    }
    this.servers = {};
};

var clazz = module.exports.prototype;

function doHash(key, rotation) {
    var hasher = crypto.createHash('sha1');
    hasher.update(key);
    hasher.update('' + rotation);
    return hasher;
}

clazz.hashIndex = function(key, rotation) {
    rotation = rotation || 0;
    var hasher = doHash(key, rotation);
    var bin = hasher.digest('binary');
    var num = 0;
    for(var i = 0; i < 4; i++) {
	num *= 256;
	num += bin.charCodeAt(i);
    }
    return num % this.size;
};

clazz.addServer = function(server) {
    for(var i = 0; i < this.mult; i++) {
	var digest = doHash(server, i).digest('base64');
	var index = this.hashIndex(server, i);
	this.array[index].push(digest);
	this.array[index].sort();
	this.servers[digest] = server;
    }
};

clazz.getServerFor = function(key) {
    for(var i = 0; i < this.size * 2; i++) {
	var index = this.hashIndex(key, i);
	if(this.array[index].length > 0) {
	    return this.servers[this.array[index][0]];
	}
    }
    throw Error('No servers');
};

clazz.removeServer = function(server) {
    for(var i = 0; i < this.mult; i++) {
	var digest = doHash(server, i).digest('base64');
	var index = this.hashIndex(server, i);
	this.array[index].splice(this.array[index].indexOf(digest), 1);
	this.array[index].sort();
	delete this.servers[digest];
    }
};
