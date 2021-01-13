## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

beam_DATA = tcap_cco_server.beam \
		tcap_dha_fsm.beam \
		tcap_ism_fsm.beam \
		tcap_tco_server.beam \
		tcap_tsm_fsm.beam \
		TR.beam \
		TC.beam \
		TCAPMessages.beam \
		DialoguePDUs.beam \
		UnidialoguePDUs.beam \
		Remote-Operations-Information-Objects.beam \
		Remote-Operations-Useful-Definitions.beam \
		Remote-Operations-Generic-ROS-PDUs.beam \
		TC-TMP.beam \
		TC-Testing-User.beam \
		TCAP-Examples.beam

%.beam:$(top_srcdir)/src/ITU/%.erl	\
		$(top_srcdir)/include/tcap.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include -I \
			$(top_builddir)/src/ITU -I $(top_builddir)/include $<

TR.beam:$(top_builddir)/src/ITU/TR.erl  \
		$(top_builddir)/src/ITU/TR.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src/ITU $<

TC.beam:$(top_builddir)/src/ITU/TC.erl  \
		$(top_builddir)/src/ITU/TC.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src/ITU $<

TCAPMessages.beam:$(top_builddir)/src/ITU/TCAPMessages.erl  \
		$(top_builddir)/src/ITU/TCAPMessages.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src/ITU $<

DialoguePDUs.beam:$(top_builddir)/src/ITU/DialoguePDUs.erl  \
		$(top_builddir)/src/ITU/DialoguePDUs.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src/ITU $<

UnidialoguePDUs.beam:$(top_builddir)/src/ITU/UnidialoguePDUs.erl  \
		$(top_builddir)/src/ITU/UnidialoguePDUs.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src/ITU $<

Remote-Operations-Information-Objects.beam:$(top_builddir)/src/ITU/Remote-Operations-Information-Objects.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src/ITU $<

Remote-Operations-Useful-Definitions.beam:$(top_builddir)/src/ITU/Remote-Operations-Useful-Definitions.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src/ITU $<

Remote-Operations-Generic-ROS-PDUs.beam:$(top_builddir)/src/ITU/Remote-Operations-Generic-ROS-PDUs.erl  \
		$(top_builddir)/src/ITU/Remote-Operations-Generic-ROS-PDUs.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src/ITU $<

TC-TMP.beam:$(top_builddir)/src/ITU/TC-TMP.erl  \
		$(top_builddir)/src/ITU/TC-TMP.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src/ITU $<

TC-Testing-User.beam:$(top_builddir)/src/ITU/TC-Testing-User.erl  \
		$(top_builddir)/src/ITU/TC-Testing-User.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src/ITU $<

TCAP-Examples.beam:$(top_builddir)/src/ITU/TCAP-Examples.erl  \
		$(top_builddir)/src/ITU/TCAP-Examples.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src/ITU $<

CLEANFILES = $(beam_DATA)

