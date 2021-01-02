## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

beam_DATA = tcap_cco_server.beam \
		tcap_dha_fsm.beam \
		tcap_ism_fsm.beam \
		tcap_tco_server.beam \
		tcap_tsm_fsm.beam

%.beam:$(top_srcdir)/src/ANSI/%.erl	$(top_srcdir)/include/tcap.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include -I $(top_builddir)/src/ANSI -I $(top_builddir)/include $<

CLEANFILES = $(beam_DATA)

