#!/bin/sh
sbcl --noinform --load output-configuration.lisp --eval '(output-configuration)' --quit

CONFIG="LISP-CONFIGURATION"

LISP_IMPLEMENTATION_TYPE=`sed -n "/^lisp-implementation-type:[ ]*\([^ ]*\)$/s//\1/p" ${CONFIG}`
LISP_IMPLEMENTATION_VERSION=`sed -n "/^lisp-implementation-version:[ ]*\([^ ]*\)$/s//\1/p" ${CONFIG}`
MINIMUM_SBCL_VERSION_FOR_INSTANS=`sed -n "/^minimum-sbcl-version-for-instans:[ ]*\([^ ]*\)$/s//\1/p" ${CONFIG}`
CONSISTENT_SBCL_VERSION=`sed -n "/^consistent-sbcl-version:[ ]*\([^ ]*\)$/s//\1/p" ${CONFIG}`
QUICKLISP=`sed -n "/^quicklisp:[ ]*\([^ ]*\)$/s//\1/p" ${CONFIG}`
ASDF=`sed -n "/^asdf:[ ]*\([^ ]*\)$/s//\1/p" ${CONFIG}`
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
    echo "Please email esko.nuutila@aalto.fi if you are able to use it with $LISP_IMPLEMENTATION_TYPE."
fi
if test "$CONSISTENT_SBCL_VERSION" = "no" ; then
    echo "You seem to be running $LISP_IMPLEMENTATION_TYPE $LISP_IMPLEMENTATION_VERSION. Instans has only been tested version $LISP_IMPLEMENTATION_TYPE $MINIMUM_SBCL_VERSION_FOR_INSTANS."
fi
if test "$QUICKLISP" = "no"; then
    echo "No Quicklisp installed."
    /bin/echo -n "Would you like me to install it now (y/n)?"
    read answer
    if test "$answer" = "y" ; then
      echo "Trying to download http://beta.quicklisp.org/quicklisp.lisp with curl"
      curl -s -O http://beta.quicklisp.org/quicklisp.lisp
      if test $? -ne 0; then
	  echo "Curl failed. Check http://www.quicklisp.org/beta/ how to install Quicklisp"
      else
	  echo "Trying to install Quicklisp into ~/quicklisp. Also updating your ~/.sbclrc file to automatically use Quicklisp"
	  sbcl --noinform --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(ql:add-to-init-file)" --quit
	  if test $? -ne 0; then
	      echo "Some problem in installing quicklisp?"
	  fi
      fi
    fi
fi
if test "$ASDF" = "no"; then
    echo "No ASDF found. It should come with $LISP_IMPLEMENTATION_TYPE. You may try to install it from http://common-lisp.net/project/asdf/ and then try again."
fi


