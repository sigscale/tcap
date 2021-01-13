## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

beam_DATA = tcap_server.beam \
		tcap_cco_server.beam \
		tcap_dha_fsm.beam \
		tcap_ism_fsm.beam \
		tcap_tco_server.beam \
		tcap_tsm_fsm.beam \
		queryWithPerm_fsm.erl \
		TR.beam \
		TCAPPackage.beam \
		TCAP-Remote-Operations-Information-Objects.beam


%.beam:$(top_srcdir)/src/ANSI/%.erl	$(top_srcdir)/include/tcap.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include -I $(top_builddir)/src/ANSI -I $(top_builddir)/include $<

TR.beam:$(top_builddir)/src/ANSI/TR.erl	$(top_builddir)/src/ANSI/TR.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src/ANSI $<

TCAPPackage.beam:$(top_builddir)/src/ANSI/TCAPPackage.erl \
		$(top_builddir)/src/ANSI/TCAPPackage.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src/ANSI $<

TCAP-Remote-Operations-Information-Objects.beam:$(top_builddir)/src/ANSI/TCAP-Remote-Operations-Information-Objects.erl \
		$(top_builddir)/src/ANSI/TCAP-Remote-Operations-Information-Objects.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src/ANSI $<

CLEANFILES = $(beam_DATA)

