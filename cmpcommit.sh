#!/bin/bash
CURRENT_BRANCH=`git branch`
BRANCH1=$1
shift
BRANCH2=$1
shift
COMMIT1=(git checkout $BRANCH1; git log | head -1 | awk '{print $2}')
COMMIT2=(git checkout $BRANCH2; git log | head -1 | awk '{print $2}')
git checkout $CURRENT_BRANCH
echo git diff $* $COMMIT1 $COMMIT2
git diff $* $COMMIT1 $COMMIT2





