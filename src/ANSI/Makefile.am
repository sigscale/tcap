## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

ASN_MODULES = TR.erl \
		TCAPPackage.erl \
		TCAP-Remote-Operations-Information-Objects.erl

erl_DATA = tcap_server.erl \
		tcap_tco_server.erl \
		tcap_cco_server.erl \
		tcap_dha_fsm.erl \
		tcap_ism_fsm.erl \
		tcap_tsm_fsm.erl \
		queryWithPerm_fsm.erl \
		$(ASN_MODULES)

ASNCFLAGS = $(ERLCFLAGS) +ber +noobj -I $(top_srcdir)/asn_src/ITU

%.erl:$(top_srcdir)/asn_src/ANSI/%.asn
	$(ERLC) $(ASNCFLAGS) $<

TCAPPackage.erl:$(top_srcdir)/asn_src/ANSI/TCAPPackage.asn \
		$(top_srcdir)/asn_src/ANSI/TCAP-Remote-Operations-Information-Objects.asn
	$(ERLC) $(ASNCFLAGS) $<

TCAP-Remote-Operations-Information-Objects.erl: \
		$(top_srcdir)/asn_src/ANSI/TCAP-Remote-Operations-Information-Objects.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Useful-Definitions.asn
	$(ERLC) $(ASNCFLAGS) $<

CLEANFILES = $(ASN_MODULES) \
		$(patsubst %.erl,%.asn1db,$(ASN_MODULES)) \
		$(patsubst %.erl,%.hrl,$(ASN_MODULES))

