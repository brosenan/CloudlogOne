"use strict";
var assert = require('assert');
var $S = require('suspend'), $R = $S.resume, $T = function(gen) { return function(done) { $S.run(gen, done); } };

var FireAndForget = require('../js/fireAndForget.js');

describe('FireAndForget', function(){
	it('should provide callbacks, and wait for all to be called', $T(function*(){
		var fnf = new FireAndForget();
		var count = 0;

		delayed(1, fnf.fork());
		delayed(1, fnf.fork());
		delayed(1, fnf.fork());
		
		yield fnf.join($R());
		assert.equal(count, 3);

		function delayed(ms, cb) {
			setTimeout(function() {
				count += 1;
				cb();
			}, ms);
		}
	}));

	it('should throw and exception when trying to fork after a join has completed.', $T(function*(){
		var fnf = new FireAndForget();

		delayed(1, fnf.fork());
		delayed(1, fnf.fork());
		delayed(1, fnf.fork());
		
		yield fnf.join($R());
		assert.throws(function() { fnf.fork(); }, /Attempting to fork after a join has completed/);

		function delayed(ms, cb) {
			setTimeout(function() {
				cb();
			}, ms);
		}
	}));
	it('should forward errors provided to fork callbacks to the join', $T(function*(){
		var fnf = new FireAndForget();

		delayed(1, fnf.fork());
		delayed(1, fnf.fork(), Error('foo'));
		delayed(1, fnf.fork());
		
		try {
			yield fnf.join($R());
			assert(false, 'Should fail');
		} catch(e) {
			assert.equal(e.message, 'foo');
		}

		function delayed(ms, cb, exception) {
			setTimeout(function() {
				cb(exception);
			}, ms);
		}
	}));
	it('should support callbacks being called before .join()', $T(function*(){
		var fnf = new FireAndForget();
		fnf.fork()();
		yield fnf.join($R());
	}));

});
