#!/bin/sh

set -e

dir=`dirname $0`
testfile=.test.pl
find $dir -name "*-test.pl" | sed -e "s%.*%:- ['&'].%" > $testfile

swipl -f $testfile -t 'run_tests.'
rm $testfile

