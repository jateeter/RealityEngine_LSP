LISP ?= sbcl
QUICKLISP ?= $(CURDIR)/quicklisp/setup.lisp

.PHONY: deps-check test build reality perception clean

deps-check:
	@test -x "$$(command -v $(LISP))" || (echo "Missing Common Lisp runtime: $(LISP)" >&2; exit 1)
	@test -f "$(QUICKLISP)" || (echo "Missing Quicklisp setup: $(QUICKLISP)" >&2; exit 1)

test: deps-check
	$(LISP) --noinform --disable-debugger --load "$(QUICKLISP)" \
	  --eval '(pushnew (truename ".") ql:*local-project-directories*)' \
	  --eval '(ql:register-local-projects)' \
	  --eval '(ql:quickload :reality-engine-lsp/tests :force t)' \
	  --eval '(asdf:test-system :reality-engine-lsp/tests)' \
	  --quit

build: deps-check
	mkdir -p bin
	$(LISP) --noinform --disable-debugger --load "$(QUICKLISP)" \
	  --eval '(pushnew (truename ".") ql:*local-project-directories*)' \
	  --eval '(ql:register-local-projects)' \
	  --eval '(ql:quickload :reality-engine-lsp :force t)' \
	  --eval '(sb-ext:save-lisp-and-die "bin/reality-engine-lsp" :toplevel #'\''reality-engine-lsp:main :executable t)' \
	  --quit

reality: deps-check
	$(LISP) --noinform --disable-debugger --load "$(QUICKLISP)" \
	  --eval '(pushnew (truename ".") ql:*local-project-directories*)' \
	  --eval '(ql:register-local-projects)' \
	  --eval '(ql:quickload :reality-engine-lsp :force t)' \
	  --eval '(reality-engine-lsp:start-reality-from-environment)' \
	  --eval '(loop (sleep 3600))'

perception: deps-check
	$(LISP) --noinform --disable-debugger --load "$(QUICKLISP)" \
	  --eval '(pushnew (truename ".") ql:*local-project-directories*)' \
	  --eval '(ql:register-local-projects)' \
	  --eval '(ql:quickload :reality-engine-lsp :force t)' \
	  --eval '(reality-engine-lsp:start-perception-from-environment)' \
	  --eval '(loop (sleep 3600))'

clean:
	rm -rf bin logs run
