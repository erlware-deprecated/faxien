# otp.mk
# - to be included in all OTP Makefiles
# installed to /usr/local/include/erlang/otp.mk

# gmake looks in /usr/local/include - that's hard-coded
# users of this file will use include erlang/top.mk

# most interface files will be installed to $ERL_RUN_TOP/app-vsn/include/*.hrl

# The erts version to use when running your release
ifndef TARGET_ERTS_VSN
TARGET_ERTS_VSN=5.5.5
endif

# The location of systemwide installed packages.
ifndef ERLWARE_HOME
ERLWARE_HOME=/usr/local/erlware
endif

# The location of the erlang runtime system.
ifndef ERL_RUN_TOP
ERL_RUN_TOP=$(ERLWARE_HOME)/erts_packages
endif

# Compile flags to be passed to erlc
ERL_COMPILE_FLAGS+=-W0

#=================================================================
# You will probably not need to change anything beneith this point
#=================================================================

# OS_TYPE is FreeBSD, NetBSD, OpenBSD, Linux, SCO_SV, SunOS. 
OS_TYPE=${shell uname}

# MHOST is the host where this Makefile runs.
MHOST=${shell hostname -s}
#
# Edit to reflect local environment.
# ifeq (${OS_TYPE},Linux)
# ERL_RUN_TOP=/usr/local/lib/erlang
#  Note* ERL_RUN_TOP can be determined by starting an 
#        erlang shell and typing code:root_dir().
# ERL_TOP=a symbolic link to the actual source top, which changes from version to version
#  Note* ERL_TOP is the directory where the erlang 
#        source files reside. Make sure to run ./configure there. 
# TARGET=i686-pc-linux-gnu
#  Note* Target can be found in $ERL_TOP/erts
# endif

# See above for directions.
ifeq (${OS_TYPE},Linux)
ERL_TOP=/opt/OTP_SRC
TARGET=i686-pc-linux-gnu
endif

ERLANG_OTP=/usr/local/erlang/otp
VAR_OTP=/var/otp


# Aliases for common binaries
# Note - CFLAGS is modified in erlang.conf


################################
# SunOS
################################
ifeq (${OS_TYPE},SunOS)

    CC=gcc
    CXX=g++
    AR=/usr/ccs/bin/ar
    ARFLAGS=-rv
    CXXFLAGS+=${CFLAGS} -I/usr/include/g++
    LD=/usr/ccs/bin/ld
    RANLIB=/usr/ccs/bin/ranlib

CFLAGS+=-Wall -pedantic -ansi -O 
CORE=*.core
endif


################################
# FreeBSD
################################
ifeq (${OS_TYPE},FreeBSD)

  ifdef LINUXBIN
    COMPAT_LINUX=/compat/linux
    CC=${COMPAT_LINUX}/usr/bin/gcc
    CXX=${COMPAT_LINUX}/usr/bin/g++
    AR=${COMPAT_LINUX}/usr/bin/ar
    ARFLAGS=-rv
    CXXFLAGS+=-fhandle-exceptions ${CFLAGS} -I${COMPAT_LINUX}/usr/include/g++
    LD=${COMPAT_LINUX}/usr/bin/ld
    RANLIB=${COMPAT_LINUX}/usr/bin/ranlib
	BRANDELF=brandelf -t Linux
  else
    CC=gcc
    CXX=g++
    AR=/usr/bin/ar
    ARFLAGS=-rv
    CXXFLAGS+=-fhandle-exceptions ${CFLAGS} -I/usr/include/g++
    LD=/usr/bin/ld
    RANLIB=/usr/bin/ranlib
	BRANDELF=@true

    ifdef USES_PTHREADS
      CFLAGS+=-D_THREAD_SAFE
      LDFLAGS+=-lc_r

	  # -pthread flag for 3.0+
	  ifneq (${shell uname -r | cut -d. -f1},2)
		CFLAGS+=-pthread
	  endif
    endif
  endif

CFLAGS+=-Wall -pedantic -ansi -O -DFREEBSD
CORE=*.core
endif

################################
# OpenBSD
################################
ifeq (${OS_TYPE},OpenBSD)

    CC=gcc
    CXX=g++
    AR=/usr/bin/ar
    ARFLAGS=-rv
    CXXFLAGS+=${CFLAGS} -I/usr/include/g++
    LD=/usr/bin/ld
    RANLIB=/usr/bin/ranlib

    ifdef USES_PTHREADS
      CFLAGS+=-D_THREAD_SAFE
      LDFLAGS+=-lc_r

	  # -pthread flag for 3.0+
	  ifneq (${shell uname -r | cut -d. -f1},2)
		CFLAGS+=-pthread
	  endif
    endif

CFLAGS+=-Wall -pedantic -ansi -O -DOPENBSD
CORE=*.core
endif

