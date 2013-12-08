#!/bin/sh
sbcl --noinform --load output-configuration.lisp --eval "(output-configuration \"$LISP_CONFIGURATION\")" --quit
LISP_IMPLEMENTATION_TYPE=`sed -n "/^lisp-implementation-type:[ ]*\([^ ]*\)$/s//\1/p" ${CONFIG}`
LISP_IMPLEMENTATION_VERSION=`sed -n "/^lisp-implementation-version:[ ]*\([^ ]*\)$/s//\1/p" ${CONFIG}`
MINIMUM_SBCL_VERSION_FOR_INSTANS=`sed -n "/^minimum-sbcl-version-for-instans:[ ]*\([^ ]*\)$/s//\1/p" ${CONFIG}`
CONSISTENT_SBCL_VERSION=`sed -n "/^consistent-sbcl-version:[ ]*\([^ ]*\)$/s//\1/p" ${CONFIG}`
QUICKLISP=`sed -n "/^quicklisp:[ ]*\([^ ]*\)$/s//\1/p" ${CONFIG}`
ASDF=`sed -n "/^asdf:[ ]*\([^ ]*\)$/s//\1/p" ${CONFIG}`
echo
echo "Detected the following configuration:"
echo "  LISP_IMPLEMENTATION_TYPE=$LISP_IMPLEMENTATION_TYPE"
echo "  LISP_IMPLEMENTATION_VERSION=$LISP_IMPLEMENTATION_VERSION"
echo "  MINIMUM_SBCL_VERSION_FOR_INSTANS=$MINIMUM_SBCL_VERSION_FOR_INSTANS"
echo "  CONSISTENT_SBCL_VERSION=$CONSISTENT_SBCL_VERSION"
echo "  QUICKLISP=$QUICKLISP"
echo "  ASDF=$ASDF"
echo
if test "$LISP_IMPLEMENTATION_TYPE" != "SBCL" -o $CONSISTENT_SBCL_VERSION = "no" ; then
    echo "You seem to be running $LISP_IMPLEMENTATION_TYPE. Instans has only been tested with SBCL (minimum version $MINIMUM_SBCL_VERSION_FOR_INSTANS)."
    exit 1
fi
if test "$CONSISTENT_SBCL_VERSION" = "no" ; then
    echo "You seem to be running $LISP_IMPLEMENTATION_TYPE $LISP_IMPLEMENTATION_VERSION. Instans has only been tested version $LISP_IMPLEMENTATION_TYPE $MINIMUM_SBCL_VERSION_FOR_INSTANS."
    exit 1
fi
if test "$QUICKLISP" = "no"; then
    echo "Installing Quicklisp"
    if test "$answer" = "y" ; then
      echo "Trying to download http://beta.quicklisp.org/quicklisp.lisp with curl"
      curl -s -O http://beta.quicklisp.org/quicklisp.lisp
      if test $? -ne 0; then
	  echo "Curl failed. Check http://www.quicklisp.org/beta/ how to install Quicklisp"
	  exit 1
      else
	  echo "Trying to install Quicklisp into ~/quicklisp. Also updating your ~/.sbclrc file to automatically use Quicklisp"
	  sbcl --noinform --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval  "(ql:add-to-init-file)" --quit
	  rm quicklisp.lisp
      fi
    else
	exit 1
    fi
fi
if test "$ASDF" = "no"; then
    echo "No ASDF found. It should come with $LISP_IMPLEMENTATION_TYPE. You may try to install it from http://common-lisp.net/project/asdf/ and then try again."
    exit 1
fi
echo
echo "Creating a custom Lisp image for Instans in file 'sbcl.core-for-instans'."
echo "After this, you should now be able to run sbcl with the special image by command:"
echo
echo "    sbcl --core sbcl.core-for-instans"
echo
echo "or by using make:"
echo
echo "make run"
echo
cd ../src
exec sbcl --eval "(progn (require (quote sb-bsd-sockets)) (require (quote sb-posix)) (require (quote sb-introspect)) (require (quote sb-cltl2)) (require (quote asdf)) (push (probe-file \".\") asdf:*central-registry*) (asdf:compile-system :instans) (asdf:load-system :instans) (in-package :instans) (save-lisp-and-die \"sbcl.core-for-instans\"))"

