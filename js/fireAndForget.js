"use strict";
module.exports = function() {
    this._count = 0;
};

var clazz = module.exports.prototype;
clazz.fork = function() {
    var self = this;
    this._count += 1;
    return function() {
	self._count -= 1;
	if(self._count == 0) {
	    self._cb();
	}
    };
};
clazz.join = function(cb) {
    this._cb = cb;
};
