"use strict";
var assert = require('assert');
var $S = require('suspend'), $R = $S.resume, $T = function(gen) { return function(done) { $S.run(gen, done); } };

var PrologInterface = require('../js/prologInterface.js');
var ChunkStore = require('../js/chunkStore.js');

var prolog = new PrologInterface();

describe('ChunkStore', function(){
    
});
