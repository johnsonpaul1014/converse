# converse
A LISP language processor

* Install Common LISP ("brew install sbcl" on OS X)
* Install QuickLisp library manager:
  curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
  sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
       --eval '(quicklisp-quickstart:install :path ".quicklisp")' \
       --quit

