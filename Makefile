.PHONY: clean
clean:
	eldev clean all

.PHONY: prepare
prepare:
	eldev -C --unstable -p -dtT prepare

.PHONY: lint
lint:
	eldev -C --unstable -T lint

.PHONY: test
test:
	eldev -C --unstable -T test

.PHONY: debug
debug:
	eldev -C --debug --unstable -T test

.PHONY: jmr
jmr:
	eldev -C --debug --unstable -T test -f test-jmr.el

docs:
	make -C doc all

html:
	make -C doc html-dir

install: install-docs

install-docs: docs
	make -C doc install-docs

install-info: info
	make -C doc install-info
