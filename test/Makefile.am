## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

testmods = tcap_test_nsap_server \
		tcap_test_usap_fsm \
		tcap_api_SUITE \
		tcap_q787_SUITE

check_DATA = .dialyzer_ok .common_test_ok

.dialyzer_ok:	$(addsuffix .beam, $(testmods))
	dialyzer --src $(top_srcdir)/src

%.beam:$(top_srcdir)/test/%.erl $(top_srcdir)/include/tcap.hrl
	$(ERLC) $(ERLCFLAGS) -pa $(top_builddir)/ebin \
			-I $(top_builddir)/src/ITU \
			-I $(top_srcdir)/include $<

.common_test_ok:	$(addsuffix .beam, $(testmods)) sys.config
	@if [ ! -d log ]; then mkdir log; fi
	@if [ ! -f ct.cfg ]; then cp $(top_srcdir)/test/ct.cfg .; fi
	@if [ ! -f test.spec ]; then cp $(top_srcdir)/test/test.spec .; fi
	ct_run -no_auto_compile \
			-pa $(top_builddir)/ebin -pa $(ERLANG_LIB_DIR_sccp)/ebin \
			-logdir log -config ct.cfg -spec test.spec -dir . \
			-erl_args -noshell -sname $(LOGNAME)-ct -config sys

sys.config:	$(top_srcdir)/test/sys.config-in
	sed -e "s#%SSN%#$$((1 + $$RANDOM + 254))#" < $< > $@

clean-local:
	-rm -r *.beam log/*

