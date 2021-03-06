dnl This is the configuration script for NIMBLE.
dnl Currently, it addresses having the header files from the Eigen software/library
dnl in non-standard locations on the machine.
dnl The idea is to generate a Makevars file in inst/make that will be copied or linked
dnl to the user's current directory or the directory in which the C++ code NIMBLE creates.


AC_INIT(DESCRIPTION)

dnl We should probably get the C++ compiler from R's configuration script to ensure compatability.
AC_PROG_CXX

NEED_MAKEVARS_FILE=TRUE

dnl The caller can specify where their Eigen/ directory is using, e.g.,
dnl    --configure-args='--with-eigen=/home/bob/local'
dnl or by setting the environment variable EIGEN_DIR
AC_ARG_WITH(eigen, [  --with-eigen            path for the directory containing Eigen/ directory with all of the #include files], EIGEN_DIR="${withval}")

AC_ARG_ENABLE(lib, [  --enable-lib            use a single shared libnimble rather than linking code into each DLL/DSO generated when compiling generated code],
                         ENABLE_LIB=${enableval}, ENABLE_LIB=true)


if test -n "${NO_LIBNIMBLE}" ; then
  ENABLE_LIB=false
  echo "Disabling libnimble."
fi


if test -n "$EIGEN_DIR" ; then
  echo "Checking eigen directory $EIGEN_DIR"
  if ! test -d "$EIGEN_DIR" ; then
     echo "$EIGEN_DIR is not a directory"
  fi

  if ! test -d "$EIGEN_DIR/Eigen" ; then
     echo "$EIGEN_DIR does not contain the directory Eigen"
     exit 1;
  fi

  if ! test -f "$EIGEN_DIR/Eigen/Dense" ; then
     echo "$EIGEN_DIR/Eigen does not contain the file Dense"
     exit 1;
  fi

  if ! test "${EIGEN_DIR}" = "" ; then
    EIGEN_INC="-I${EIGEN_DIR}"
  fi
fi


if test -n "$EIGEN_DIR" && ! test "${EIGEN_DIR}" = "" ; then
   echo "Setting CPPFLAGS to find Eigen"
   export CPPFLAGS="-I${EIGEN_DIR}"
fi

AC_LANG([C++])
dnl  AC_LANG_PUSH([C++]);

AC_CHECK_HEADERS([Eigen/Dense], 
                 [echo "Found Eigen/Dense";
                  rm -rf inst/Eigen_local], 
                 [echo "Cannot find Eigen/Dense include file.";
                  echo "Using the version of Eigen provided locally with the R package";
                  echo "To avoid this, re-install and specify the path containing the Eigen/ directory via";
                  echo "   --with-eigen (see INSTALL for syntax) or set the environment variable EIGEN_DIR"; 
                  mv inst/Eigen_local inst/include/Eigen ;
                  EIGEN_DIR=""
                  ])


if test -n "$EIGEN_DIR" ; then
echo "Using Eigen directory ${EIGEN_DIR}"
EIGEN_INC="-I$EIGEN_DIR"
fi


AC_SUBST(EIGEN_INC)
AC_SUBST(NEED_MAKEVARS_FILE)

NIMBLE_DIR=${R_PACKAGE_DIR}
echo "NIMBLE_DIR = ${NIMBLE_DIR}"
AC_SUBST(NIMBLE_DIR)

NIMBLE_INC_DIR=${NIMBLE_DIR}/include
AC_SUBST(NIMBLE_INC_DIR)

if test "${ENABLE_LIB}" = "yes" ; then
 ENABLE_LIB="TRUE"
elif test "${ENABLE_LIB}" = "no" ; then
  ENABLE_LIB="FALSE"
fi

R_ENABLE_LIB=`echo "${ENABLE_LIB}" | tr "[a-z]" "[A-Z]"`
echo "ENABLE_LIB=${ENABLE_LIB}"
AC_SUBST(R_ENABLE_LIB)



if ! test `uname` = "Darwin" ; then
RPATH="-Wl,-rpath=${NIMBLE_DIR}/CppCode"
fi
AC_SUBST(RPATH)

if test "${ENABLE_LIB}" != "false" ; then
  PKG_LIBS="-L../inst/CppCode -lnimble $RPATH"
fi
AC_SUBST(PKG_LIBS)

USE_REGISTRATION=".NimbleUseRegistration = TRUE"
AC_SUBST(USE_REGISTRATION)

AC_OUTPUT(inst/make/Makevars inst/make/Makevars_lib R/config.R src/Makevars)



if test "${ENABLE_LIB}" != "false" ; then
 echo "Building libnimble"
 (cd inst/CppCode ; ${MAKE} )
else 
#  mkdir inst/CppCode
  cp inst/CppCode/*.cpp src
fi
