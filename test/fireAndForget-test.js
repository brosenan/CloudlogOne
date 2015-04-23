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
});
