
DE.SETF.GRAPHICS: how to build it with Steel Bank Common Lisp
------------

In order to use asdf with sbcl, 

The system can be built and saved from the command line

    $ sbcl --userinit readmes/build-init.lisp \
      --eval "(asdf:operate 'asdf:load-op :de.setf.graphics.clx)" \
      --eval "(asdf:operate 'asdf:load-op :de.setf.graphics.tests)" \
      --eval '(sb-ext:save-lisp-and-die "sbcl-graphics.core")'

In order to get the tests, the `de.setf.graphics.tests` module must be loaded as well.
nb. asdf requires the changes in [asdf.diff](./asdf.diff)

Start it with the core

    $ sbcl --core sbcl-graphics.core
    This is SBCL 1.0.35, an implementation of ANSI Common Lisp.
    More information about SBCL is available at <http://www.sbcl.org/>.
    
    SBCL is free software, provided as is, with absolutely no warranty.
    It is mostly in the public domain; some portions are provided under
    BSD-style licenses.  See the CREDITS and COPYING files in the
    distribution for more information.
    * (in-package :dsg.i)
    
    #<PACKAGE "DE.SETF.GRAPHICS.IMPLEMENTATION">
    * (initialize-clx-tests)

    #<CLX-CONTEXT {12CD1F19}>
    * (with-projection-context (*clx-c*)
        (initialize-test-context *clx-c*)
        (run-life :cycles 256 :sleep nil :initialize-p t :size 512))

    T
    * (time (with-projection-context (*clx-c*)
              (initialize-test-context *clx-c*)
                (test-sampler-animation :count 100 :sleep nil)))

    Evaluation took:
      4.860 seconds of real time
      2.408786 seconds of total run time (2.117926 user, 0.290860 system)
      [ Run times consist of 0.012 seconds GC time, and 2.397 seconds non-GC time. ]
      49.57% CPU
      24,081,296 bytes consed
  
    T
    * 


A snap-shot core is present in the downloads section.
