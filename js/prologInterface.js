"use strict";
var spawn = require('child_process').spawn;
var fs = require('fs');

var byline = require('byline');
var EventEmitter = require('events').EventEmitter;

var $S = require('suspend'), $R = $S.resume, $T = function(gen) { return function(done) { $S.run(gen, done); } };

var upstreamRegex = /([^ ]+,[^ ]+) (.*)/;
var persistRegex = /([^ ]+) (.*)/;

module.exports = function(logfile) {
	this.prolog = spawn('swipl', ['-f', __dirname + '/../prolog/main.pl', '-t', 'cloudlog1']);
	if(logfile) {
		this._log = fs.createWriteStream(logfile);
		this.prolog.stderr.pipe(this._log);
		this.prolog.stdout.pipe(this._log);
	}
	this.prolog.stdout.setEncoding('utf-8');
	this.lines = byline(this.prolog.stdout);
	var self = this;
	this.emitter = null;
	this.queue = [];
	var key;
	this.lines.on('data', function(data) {
		if(data.substr(0, 2) === '! ') {
			self.emitter.emit('error', new Error(data.substr(2)));
			self.done();
		} else if(data.substr(0, 2) === '. ') {
			self.emitter.emit('success', data.substr(2));
			self.done();
		} else if(data.substr(0, 2) === ': ') {
			self.emitter.emit('downstream', data.substr(2));
		} else if(data.substr(0, 2) === '@ ') {
			key = data.substr(2);
		} else if(data.substr(0, 2) === '$ ') {
			self.emitter.emit('client-patch', data.substr(2));
		} else if(data.substr(0, 2) === '? ') {
			let m = data.substr(2).match(upstreamRegex);
			if(m == null) {
				throw Error('Bad upstream response: ' + data);
			}
			self.emitter.emit('upstream', m[1], key, m[2]);
			key = undefined;
		} else if(data.substr(0, 2) === '& ') {
			let m = data.substr(2).match(persistRegex);
			if(m == null) {
				throw Error('Bad persist response: ' + data);
			}
			self.emitter.emit('persist', m[1], m[2]);
		}
	});
};

module.exports.prototype.request = function(req) {
	if(this._log) {
		this._log.write('> ' + req + '\n');
	}
	if(this.emitter == null) {
		this.send(req);
		this.emitter = new EventEmitter();
		return this.emitter;
	} else {
		var newEmitter = new EventEmitter();
		newEmitter.req = req;
		this.queue.push(newEmitter);
		return newEmitter;
	}
};

module.exports.prototype.done = function() {
	this.emitter.emit('done');
	if(this.queue.length > 0) {
		this.emitter = this.queue.shift();
		this.send(this.emitter.req);
	} else {
		this.emitter = null;
	}
};

module.exports.prototype.send = function(req) {
	this.prolog.stdin.write(req + '.\n');
};

module.exports.prototype.stop = function(cb) {
	this.prolog.stdin.end(cb);
};
