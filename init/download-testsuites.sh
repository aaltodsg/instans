#!/bin/sh
ROOT=`dirname $0`
cd $ROOT/../tests
if test -d data-r2; then
    echo "data-r2 already exists, skipping downloading it"
else
    echo "Downloading http://www.w3.org/2001/sw/DataAccess/tests/data-r2.tar.gz"
    curl -s -O http://www.w3.org/2001/sw/DataAccess/tests/data-r2.tar.gz
    tar -xzf data-r2.tar.gz test-suite-archive/data-r2
    echo "Moving test-suite-archive/data-r2 to data-r2"
    mv  test-suite-archive/data-r2 .
    rm -rf testsuite-archive data-r2.tar.gz
fi
if test -d data-sparql11; then
    echo "data-sparql11 already exists, skipping downloading it"
else
    echo "Downloading http://www.w3.org/2009/sparql/docs/tests/sparql11-test-suite-20121023.tar.gz"
    curl -s -O http://www.w3.org/2009/sparql/docs/tests/sparql11-test-suite-20121023.tar.gz
    tar -xzf sparql11-test-suite-20121023.tar.gz
    echo "Moving sparql11-test-suite to data-sparql11"
    mv sparql11-test-suite data-sparql11
    rm sparql11-test-suite-20121023.tar.gz
fi
