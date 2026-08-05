#!/usr/bin/env bash
# Install Quicklisp and warm this system's dependencies.
#
# quicklisp/ is not tracked in this repository, so a fresh checkout — a CI
# runner, a new laptop, a container — has no Quicklisp and `start.sh` exits with
# "Missing Quicklisp." This script closes that gap and is the supported way to
# prepare a clean machine.
#
# Idempotent: an existing installation is verified and reused unless --force.
#
# Usage:
#   scripts/bootstrap-quicklisp.sh [--force] [--home] [--no-warm]
#
#   --force     reinstall even if setup.lisp already exists
#   --home      install to $HOME/quicklisp instead of the repo-local quicklisp/
#               (start.sh checks repo-local first, then $HOME)
#   --no-warm   skip `ql:quickload :reality-engine-lsp`; only install Quicklisp
#
# Env:
#   LISP                 Lisp binary (default: sbcl)
#   QUICKLISP_DIST_URL   quicklisp.lisp source (default: the official beta URL)
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

LISP="${LISP:-sbcl}"
QUICKLISP_DIST_URL="${QUICKLISP_DIST_URL:-https://beta.quicklisp.org/quicklisp.lisp}"

FORCE=false
WARM=true
TARGET_DIR="$ROOT_DIR/quicklisp"

while [ $# -gt 0 ]; do
  case "$1" in
    --force)   FORCE=true ;;
    --home)    TARGET_DIR="$HOME/quicklisp" ;;
    --no-warm) WARM=false ;;
    -h|--help) sed -n '2,25p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "Unknown option: $1" >&2; exit 2 ;;
  esac
  shift
done

SETUP="$TARGET_DIR/setup.lisp"

if ! command -v "$LISP" >/dev/null 2>&1; then
  echo "Missing Common Lisp runtime: $LISP" >&2
  echo "  Debian/Ubuntu: sudo apt-get install -y sbcl" >&2
  echo "  macOS:         brew install sbcl" >&2
  exit 1
fi

if [ -f "$SETUP" ] && [ "$FORCE" = false ]; then
  echo "Quicklisp already installed at $SETUP"
else
  if [ "$FORCE" = true ] && [ -d "$TARGET_DIR" ]; then
    echo "--force: removing $TARGET_DIR"
    rm -rf "$TARGET_DIR"
  fi

  workdir="$(mktemp -d)"
  trap 'rm -rf "$workdir"' EXIT

  echo "Downloading $QUICKLISP_DIST_URL"
  curl -fsSL -o "$workdir/quicklisp.lisp" "$QUICKLISP_DIST_URL"

  # Recorded rather than pinned: the installer is served from the upstream
  # beta URL and is not versioned, so a pinned hash would break on every
  # upstream refresh. Logging it makes an unexpected change visible in CI logs.
  if command -v shasum >/dev/null 2>&1; then
    echo "quicklisp.lisp sha256: $(shasum -a 256 "$workdir/quicklisp.lisp" | awk '{print $1}')"
  elif command -v sha256sum >/dev/null 2>&1; then
    echo "quicklisp.lisp sha256: $(sha256sum "$workdir/quicklisp.lisp" | awk '{print $1}')"
  fi

  echo "Installing Quicklisp to $TARGET_DIR"
  "$LISP" --noinform --disable-debugger --non-interactive \
    --load "$workdir/quicklisp.lisp" \
    --eval "(quicklisp-quickstart:install :path \"$TARGET_DIR/\")" \
    --quit

  [ -f "$SETUP" ] || { echo "Install finished but $SETUP is missing" >&2; exit 1; }
  echo "Installed: $SETUP"
fi

if [ "$WARM" = false ]; then
  echo "Skipping dependency warm (--no-warm)"
  exit 0
fi

# Pull the system's dependencies now so the first engine start does not pay for
# it — and so a dependency that cannot resolve fails here, with a clear message,
# rather than inside a backgrounded start.sh whose log nobody is watching.
echo "Warming :reality-engine-lsp dependencies"
"$LISP" --noinform --disable-debugger --non-interactive \
  --load "$SETUP" \
  --eval "(pushnew (truename \"$ROOT_DIR/\") ql:*local-project-directories*)" \
  --eval "(handler-case (ql:register-local-projects) (error (c) (format t \"~&Warning: register-local-projects: ~a~%\" c)))" \
  --eval "(ql:quickload :reality-engine-lsp)" \
  --quit

echo "Quicklisp bootstrap complete."
echo "start.sh resolves: $SETUP"
