#!/bin/bash
function die () {
    echo $*
    exit 1
}

function usage() {
    die "Usage: $0 { (--clean | clean | -c | --help | help | -h | --dynamic-space-size <megabytes>) }"
}

function help() {
    cat <<EOF
--clean | clean | -c		Make a clean install. Removes old 
EOF
}

cd `dirname $0`
ROOT=`pwd`
SRC=${ROOT}/src
INIT=${ROOT}/init
BIN=${ROOT}/bin
LISP_CONFIGURATION=${INIT}/lisp-configuration
EXECUTABLE=${BIN}/instans.bin
EXECUTABLEOPTIONS=${BIN}/instans.bin.options

#echo "In directory $ROOT: running `basename $0` $*"
LAST=`echo $ROOT | sed '/^.*[/]\([^/]*\)^/s//\1/'`

if test -f "$EXECUTABLEOPTIONS"; then
    . "$EXECUTABLEOPTIONS"
    PREV_DYNAMIC_SPACE_SIZE=$DYNAMIC_SPACE_SIZE
fi

CLEAN="false"
DYNAMIC_SPACE_SIZE=

if test -f configure.options; then
    . configure.options
fi

test "$LAST" != instans || die "You should run this script in the instans root directory!"

while test $# -gt 0; do
    if test "$1" = "clean" -o "$1" = "--clean" -o "$1" = "-c"; then
	CLEAN="true"
    elif test "$1" = "--dynamic-space-size"; then
	if test $# -gt 0; then
	    DYNAMIC_SPACE_SIZE=$2
	    shift
	else
	    usage
	fi
    else
	usage
    fi
    shift
done
SBCLOPTS=""
if test "$DYNAMIC_SPACE_SIZE"; then
    SBCLOPTS="--dynamic-space-size $DYNAMIC_SPACE_SIZE"
fi

if test "$CLEAN" = "true"; then
    rm -rf $HOME/quicklisp
    rm -rf buildapp*
    NEEDSCOMPILE=true
    NEEDSBUILDAPP=true
else
    NEEDSCOMPILE=false
    NEEDSBUILDAPP=false
    newersources=`find src/ -name '*.lisp' -newer bin/instans.bin|wc -l`
    if test $newersources -ne 0; then
	NEEDSCOMPILE=true
	NEEDSBUILDAPP=true
    fi
    if test "$PREV_DYNAMIC_SPACE_SIZE" != "$DYNAMIC_SPACE_SIZE"; then
	NEEDSBUILDAPP=true
    fi
fi
if test $NEEDSCOMPILE = false -a $NEEDSBUILDAPP = false; then
    echo "No need to configure"
    exit 
fi
sbcl --noinform --load init/output-configuration.lisp --eval "(output-configuration \"$LISP_CONFIGURATION\")" --quit

LISP_IMPLEMENTATION_TYPE=`sed -n "/^lisp-implementation-type:[ ]*\([^ ]*\)$/s//\1/p" ${LISP_CONFIGURATION}`
LISP_IMPLEMENTATION_VERSION=`sed -n "/^lisp-implementation-version:[ ]*\([^ ]*\)$/s//\1/p" ${LISP_CONFIGURATION}`
MINIMUM_SBCL_VERSION_FOR_INSTANS=`sed -n "/^minimum-sbcl-version-for-instans:[ ]*\([^ ]*\)$/s//\1/p" ${LISP_CONFIGURATION}`
CONSISTENT_SBCL_VERSION=`sed -n "/^consistent-sbcl-version:[ ]*\([^ ]*\)$/s//\1/p" ${LISP_CONFIGURATION}`
QUICKLISP=`sed -n "/^quicklisp:[ ]*\([^ ]*\)$/s//\1/p" ${LISP_CONFIGURATION}`
ASDF=`sed -n "/^asdf:[ ]*\([^ ]*\)$/s//\1/p" ${LISP_CONFIGURATION}`
rm ${LISP_CONFIGURATION}

function show_configuration () {
    echo
    echo "Detected the following configuration:"
    echo "  LISP_IMPLEMENTATION_TYPE=$LISP_IMPLEMENTATION_TYPE"
    echo "  LISP_IMPLEMENTATION_VERSION=$LISP_IMPLEMENTATION_VERSION"
    echo "  MINIMUM_SBCL_VERSION_FOR_INSTANS=$MINIMUM_SBCL_VERSION_FOR_INSTANS"
    echo "  CONSISTENT_SBCL_VERSION=$CONSISTENT_SBCL_VERSION"
    echo "  QUICKLISP=$QUICKLISP"
    echo "  ASDF=$ASDF"
    echo
}
if test "$LISP_IMPLEMENTATION_TYPE" != "SBCL" -o $CONSISTENT_SBCL_VERSION = "no" ; then
    show_configuration
    die "You seem to be running $LISP_IMPLEMENTATION_TYPE. Instans has only been tested with SBCL (minimum version $MINIMUM_SBCL_VERSION_FOR_INSTANS)."
fi
if test "$CONSISTENT_SBCL_VERSION" = "no" ; then
    show_configuration
    die "You seem to be running $LISP_IMPLEMENTATION_TYPE $LISP_IMPLEMENTATION_VERSION. Instans has only been tested version $LISP_IMPLEMENTATION_TYPE $MINIMUM_SBCL_VERSION_FOR_INSTANS."
fi
if test "$QUICKLISP" = "no"; then
    if test -f $HOME/.sbclrc; then
	dt=`date "+%Y-%m-%d%T%H%M%S"`
	mv $HOME/.sbclrc $HOME/.sbclrc.$dt
    fi
    touch $HOME/.sbclrc
    echo
    echo "Installing quicklisp into ~/quicklisp"
    /bin/echo -n "  Downloading http://beta.quicklisp.org/quicklisp.lisp with curl... "
    (curl -s -O http://beta.quicklisp.org/quicklisp.lisp > /dev/null 2>&1; echo " Done.") || die " Failed! Check http://www.quicklisp.org/beta/ how to install Quicklisp."
    /bin/echo -n "  Setting up quicklisp and updating your ~/.sbclrc file to automatically use quicklisp..."
    (sbcl --noinform --load quicklisp.lisp \
 	--eval "(quicklisp-quickstart:install)" \
 	--eval  "(let ((ql-util::*do-not-prompt* t) (*query-io* (make-two-way-stream *standard-input* *standard-output*))) (ql:add-to-init-file))"  --quit > /dev/null 2>&1; echo " Done."; rm quicklisp.lisp)|| die " Failed!"
fi
test "$ASDF" = "yes" || die "No ASDF found. It should come with $LISP_IMPLEMENTATION_TYPE. You may try to install it from http://common-lisp.net/project/asdf/ and then try again."
BUILDAPP=${ROOT}/buildapp/buildapp
if test ! -x $BUILDAPP; then
    echo
    echo "Installing Buildapp."
    /bin/echo -n "  Downloading http://www.xach.com/lisp/buildapp.tgz with curl..."
    (curl -s -O http://www.xach.com/lisp/buildapp.tgz > /dev/null 2>&1; echo " Done.") || die " Failed! Check http://www.xach.com/lisp/buildapp for instructions to install buildapp"
    tar -xzf buildapp.tgz
    ln -s buildapp-* buildapp
    /bin/echo -n "  Building buildapp..."
    ((cd buildapp; make) > /dev/null 2>&1; echo " Done.") || die " Failed!"
fi
if test $NEEDSCOMPILE = true; then
    echo
    /bin/echo -n "Compiling system..."
    cd $SRC
    TMP=make_tmp_$$
    if test "$CLEAN" = "true"; then
	make clean > /dev/null 2>&1
    fi
    make 2>&1 | tee $TMP | while read line; do /bin/echo -n "."; done
    if test $? -ne 0; then
	die " Failed!"
	cat $TMP
	rm $TMP
    else
	echo " Done."
	rm $TMP
    fi
    cd $ROOT
fi
if test $NEEDSBUILDAPP = true; then
    if test "$SBCLOPTS" != ""; then
	/bin/echo -n "Building $EXECUTABLE with options$SBCLOPTS..."
    else
	/bin/echo -n "Building $EXECUTABLE..."
    fi
    ($BUILDAPP --output $EXECUTABLE $SBCLOPTS \
	       --entry instans:main \
	       --load $HOME/quicklisp/setup.lisp \
	       --asdf-path $SRC \
	       --require sb-bsd-sockets \
	       --require sb-posix \
	       --require sb-introspect \
	       --require sb-cltl2 \
	       --require asdf \
	       --eval "(setf asdf:*central-registry* (list \"$SRC/\"))" \
               --load-system instans \
               --eval '(in-package :instans)'  > /dev/null 2>&1; echo " Done.") || die " Failed!"
    cat <<EOF > $EXECUTABLEOPTIONS
DYNAMIC_SPACE_SIZE=$DYNAMIC_SPACE_SIZE
EOF
fi
echo
echo "INSTANS built."
echo
echo "Run INSTANS on command line by"
echo
echo "    instans/bin/instans [INSTANS arguments]"
echo
