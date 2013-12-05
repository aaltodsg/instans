#!/bin/sh
LOG=log.config
rm -f $LOG
touch $LOG
function msg () {
    echo $* >> $LOG
    echo $*
}
function tmp_to_log () {
    cat $TMP >> $LOG
    cat $TMP
    rm $TMP
}
msg "Logging output to $LOG"
TMP=$$_tmp
sbcl --noinform --load output-configuration.lisp --eval "(with-open-file (str \"$TMP\" :direction :output :if-exists :supersede) (let ((*error-output* str) (*standard-output* str)) (output-configuration)))" --quit
tmp_to_log
CONFIG="LISP-CONFIGURATION"
LISP_IMPLEMENTATION_TYPE=`sed -n "/^lisp-implementation-type:[ ]*\([^ ]*\)$/s//\1/p" ${CONFIG}`
LISP_IMPLEMENTATION_VERSION=`sed -n "/^lisp-implementation-version:[ ]*\([^ ]*\)$/s//\1/p" ${CONFIG}`
MINIMUM_SBCL_VERSION_FOR_INSTANS=`sed -n "/^minimum-sbcl-version-for-instans:[ ]*\([^ ]*\)$/s//\1/p" ${CONFIG}`
CONSISTENT_SBCL_VERSION=`sed -n "/^consistent-sbcl-version:[ ]*\([^ ]*\)$/s//\1/p" ${CONFIG}`
QUICKLISP=`sed -n "/^quicklisp:[ ]*\([^ ]*\)$/s//\1/p" ${CONFIG}`
ASDF=`sed -n "/^asdf:[ ]*\([^ ]*\)$/s//\1/p" ${CONFIG}`
msg
msg "Detected the following configuration:"
msg '  LISP_IMPLEMENTATION_TYPE=$LISP_IMPLEMENTATION_TYPE'
msg '  LISP_IMPLEMENTATION_VERSION=$LISP_IMPLEMENTATION_VERSION'
msg '  MINIMUM_SBCL_VERSION_FOR_INSTANS=$MINIMUM_SBCL_VERSION_FOR_INSTANS'
msg '  CONSISTENT_SBCL_VERSION=$CONSISTENT_SBCL_VERSION'
msg '  QUICKLISP=$QUICKLISP'
msg '  ASDF=$ASDF'
msg
if test "$LISP_IMPLEMENTATION_TYPE" != "SBCL" -o $CONSISTENT_SBCL_VERSION = "no" ; then
    msg "You seem to be running $LISP_IMPLEMENTATION_TYPE. Instans has only been tested with SBCL (minimum version $MINIMUM_SBCL_VERSION_FOR_INSTANS)."
    exit 1
fi
if test "$CONSISTENT_SBCL_VERSION" = "no" ; then
    msg "You seem to be running $LISP_IMPLEMENTATION_TYPE $LISP_IMPLEMENTATION_VERSION. Instans has only been tested version $LISP_IMPLEMENTATION_TYPE $MINIMUM_SBCL_VERSION_FOR_INSTANS."
    exit 1
fi
if test "$QUICKLISP" = "no"; then
    msg "No Quicklisp installed."
    /bin/echo -n "Would you like me to install it now (y/n)?"
    read answer
    if test "$answer" = "y" ; then
      msg "Trying to download http://beta.quicklisp.org/quicklisp.lisp with curl"
      curl -s -O http://beta.quicklisp.org/quicklisp.lisp
      if test $? -ne 0; then
	  msg "Curl failed. Check http://www.quicklisp.org/beta/ how to install Quicklisp"
	  exit 1
      else
	  msg "Trying to install Quicklisp into ~/quicklisp. Also updating your ~/.sbclrc file to automatically use Quicklisp"
	  sbcl --noinform --load quicklisp.lisp --eval "(with-open-file (str \"$TMP\" :direction :output :if-exists :supersede) (let ((*error-output* str) (*standard-output* str)) (quicklisp-quickstart:install)))" --eval  "(ql:add-to-init-file)" --quit
	  tmp_to_log
      fi
    else
	exit 1
    fi
fi
if test "$ASDF" = "no"; then
    msg "No ASDF found. It should come with $LISP_IMPLEMENTATION_TYPE. You may try to install it from http://common-lisp.net/project/asdf/ and then try again."
    exit 1
fi
msg
msg "Creating a custom Lisp image for Instans in file 'sbcl.core-for-instans'."
msg "After this, you should now be able to run sbcl with the special image by command:"
msg
msg "    sbcl --core sbcl.core-for-instans"
msg
msg "or by using make:"
msg
msg "make run-instans"
msg
exec sbcl --eval "(with-open-file (str \"$LOG\" :direction :output :if-exists :append) (let ((*error-output* str) (*standard-output* str)) (require 'sb-bsd-sockets) (require 'sb-posix) (require 'sb-introspect) (require 'sb-cltl2) (require 'asdf) (push (probe-file \".\") asdf:*central-registry*) (asdf:compile-system :instans) (asdf:load-system :instans) (in-package :instans) (save-lisp-and-die \"sbcl.core-for-instans\")))"
