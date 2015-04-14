"use strict";
var spawn = require('child_process').spawn;
var byline = require('byline');
var EventEmitter = require('events').EventEmitter;

var $S = require('suspend'), $R = $S.resume, $T = function(gen) { return function(done) { $S.run(gen, done); } };

module.exports = function() {
    this.prolog = spawn('swipl', ['-f', __dirname + '/../prolog/main.pl', '-t', 'cloudlog1']);
    
    //this.prolog.stderr.pipe(process.stderr);
    //this.prolog.stdout.pipe(process.stdout);
    
    this.prolog.stdout.setEncoding('utf-8');
    this.lines = byline(this.prolog.stdout);
    var self = this;
    this.emitter = null;
    this.queue = [];
    this.lines.on('data', function(data) {
	if(data.substr(0, 2) === '! ') {
	    self.emitter.emit('error', new Error(data.substr(2)));
	    self.done();
	} else if(data.substr(0, 2) === '. ') {
	    self.emitter.emit('success', data.substr(2));
	    self.done();
	} else if(data.substr(0, 2) === ': ') {
	    self.emitter.emit('downstream', data.substr(2));
	}
    });
};

module.exports.prototype.request = function(req) {
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
