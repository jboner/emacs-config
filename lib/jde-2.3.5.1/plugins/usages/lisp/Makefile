EMACS=emacs
CEDET_PATH=../../../cedet-cvs/cedet
LOADPATH=../../../jde/lisp ../../../elib  ../../../ecb-2.31 ../tests/lisp



lisp: clean 
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	@echo "(load-file \"${CEDET_PATH}/common/cedet.el\")" >> $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(require 'jde)" >> $@-compile-script
	@echo "(require 'jde-usages-etest)" >> $@-compile-script
	@echo "(setq debug-on-error t)" >> $@-compile-script
	$(EMACS) -batch --no-site-file -l $@-compile-script -f batch-byte-compile $(wildcard *.el)

clean:
	rm -f *.elc
