#!/bin/sh
## basic shell functions for discovering NAS-T dependencies

# from sbcl/find-gnumake.sh
find_make() {
  # the GNU dialect of "make" -- easier to find or port it than to
  # try to figure out how to port to the local dialect...
  if [ "$MAKE" != "" ] ; then
    # The user is evidently trying to tell us something.
    MAKE="$MAKE"
  elif [ "GNU Make" = "`make -v 2>/dev/null | head -n 1 | cut -b 1-8`" ]; then
    MAKE=make
  elif [ -x "`command -v gmake`" ] ; then
    # "gmake" is the preferred name in *BSD.
    MAKE=gmake
  elif [ -x "`command -v gnumake`" ] ; then
    # MacOS X aka Darwin
    MAKE=gnumake
  else
    echo "GNU Make not found. Try setting env var MAKE or install"
    exit 1
  fi
  export MAKE
  echo "//MAKE=\"$MAKE\""
}

find_sbcl() {
  if [ -x "`command -v sbcl`" ]; then
    echo "sbcl: OK"
    SBCL=sbcl
  else
    echo "SBCL not found. Try setting env var SBCL or install"
    exit 1
  fi
  export SBCL
  echo "//SBCL=\"$SBCL\""    
}

find_cargo() {
  if [ -x "`command -v cargo`" ]; then
    echo "cargo: OK"
    CARGO=cargo
  else
    echo "Cargo not found. Try setting env var CARGO or install from rustup"
    exit 1
  fi
  export CARGO
  echo "//CARGO=\"$CARGO\""
}

### DEV
find_hg() {
  if [ -x "`command -v hg`" ]; then
    echo "hg: OK"
    HG=hg
  else
    echo "Mercurial not found. Try setting env var HG or install"
    exit 1
  fi
  export HG
  echo "//HG=\"$HG\""    
}

find_emacs() {
  if [ -x "`command -v emacs`" ]; then
    echo "emacs: OK"
    EMACS=emacs
    if [ -x "`command -v emacsclient`" ]; then
      echo "emacsclient: OK"
      EMACSCLIENT=emacsclient
    fi
  else
    echo "Emacs not found. Try setting env var EMACS & EMACSCLIENT or install"
    exit 1
  fi
  export EMACS
  echo "//EMACS=\"$EMACS\""
  export EMACSCLIENT
  echo "//EMACSCLIENT=\"$EMACSCLIENT\""
}

####  dev/web
find_trunk() {
  if [ -x "`command -v trunk`" ]; then
    echo "trunk: OK"
    TRUNK=trunk
  else
    echo "Trunk not found. Try setting env var TRUNK or install with 'cargo install trunk'"
    exit 1
  fi
  export TRUNK
  echo "//TRUNK=\"$TRUNK\""
}

find_npm() {
  if [ -x "`command -v npm`" ]; then
    echo "npm: OK"
    NPM=npm
  else
    echo "NPM not found. Try setting env var NPM or install Node.js"
    exit 1
  fi
  export NPM
  echo "//NPM=\"$NPM\""
}

find_wasm_opt() {
  if [ -x "`command -v wasm-opt`" ]; then
    echo "wasm-opt: OK"
    WASM_OPT=wasm-opt
  else
    echo "wasm-opt not found. Try setting env var WASM_OPT or install with 'cargo install wasm-opt'"
    exit 1
  fi
  export WASM_OPT
  echo "//WASM_OPT=\"$WASM_OPT\""
}

#### dev/virt
find_podman() {
  if [ -x "`command -v podman`" ]; then
    echo "podman: OK"
    PODMAN=podman
  else
    echo "Podman not found. Try setting env var PODMAN or install"
    exit 1
  fi
  export PODMAN
  echo "//PODMAN=\"$PODMAN\""
}

find_deps() {
  find_make
  find_sbcl
  find_cargo
}
find_all_deps() {
  find_deps
  find_hg
  find_emacs
  find_trunk
  find_npm
  find_wasm_opt
  find_podman
}

$*
