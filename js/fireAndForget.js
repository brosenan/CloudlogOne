"use strict";
module.exports = function() {
    this._count = 0;
    this._joined = false;
};

var clazz = module.exports.prototype;
clazz.fork = function() {
    if(this._joined) {
	throw Error("Attempting to fork after a join has completed");
    }
    var self = this;
    this._count += 1;
    return function(err) {
	if(err) {
	    return self._cb(err);
	}
	self._count -= 1;
	if(self._count == 0) {
	    self._joined = true;
	    self._cb();
	}
    };
};
clazz.join = function(cb) {
    this._cb = cb;
};
