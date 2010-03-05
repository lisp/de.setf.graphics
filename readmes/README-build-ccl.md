
DE.SETF.GRAPHICS: build it with Clozure Common Lisp
----------------

The system can be built and saved as a run-time image from the command line

    $ export CCL=/Development/Applications/LISP/ccl-1.4/dppccl
    $ $CCL --no-init --load readmes/build-init.lisp \
      --eval "(asdf:operate 'asdf:load-op :de.setf.graphics.clx)" \
      --eval "(asdf:operate 'asdf:load-op :de.setf.graphics.tests)" \
      --eval '(ccl:save-application "ccl-graphics.image")'

Start it with the core

    $ $CCL -I ccl-graphics.image
    ;Loading #P"P-LIBRARY:net;common-lisp;asdf;asdf..newest"...
    Welcome to Clozure Common Lisp Version 1.4-r13119  (DarwinPPC32)!
    * (in-package :dsg.i)
    
    #<PACKAGE "DE.SETF.GRAPHICS.IMPLEMENTATION">
    ? (initialize-clx-tests)
    #<CLX-CONTEXT #x8BC8A4E>
    ? (with-projection-context (*clx-c*)
            (initialize-test-context *clx-c*)
            (run-life :cycles 256 :sleep nil :initialize-p t :size 64))
    T
    ? (time (with-projection-context (*clx-c*)
                  (initialize-test-context *clx-c*)
                    (test-sampler-animation :count 100 :sleep nil)))
    (WITH-PROJECTION-CONTEXT (*CLX-C*) (INITIALIZE-TEST-CONTEXT *CLX-C*) (TEST-SAMPLER-ANIMATION :COUNT 100 :SLEEP NIL)) took 3,949 milliseconds (3.949 seconds) to run 
                        with 2 available CPU cores.
    During that period, 1,957 milliseconds (1.957 seconds) were spent in user mode
                        135 milliseconds (0.135 seconds) were spent in system mode
    85 milliseconds (0.085 seconds) was spent in GC.
     37,164,160 bytes of memory allocated.
     20 minor page faults, 173 major page faults, 0 swaps.
    T


A snap-shot image is present in the downloads section.
