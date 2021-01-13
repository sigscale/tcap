## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

ASN_MODULES = TR.erl \
		TC.erl \
		TCAPMessages.erl \
		DialoguePDUs.erl \
		UnidialoguePDUs.erl \
		Remote-Operations-Information-Objects.erl \
		Remote-Operations-Useful-Definitions.erl \
		Remote-Operations-Generic-ROS-PDUs.erl \
		TC-TMP.erl \
		TC-Testing-User.erl \
		TCAP-Examples.erl

erl_DATA = tcap_tco_server.erl \
		tcap_cco_server.erl \
		tcap_dha_fsm.erl \
		tcap_tsm_fsm.erl \
		tcap_ism_fsm.erl \
		$(ASN_MODULES)

ASNCFLAGS = $(ERLCFLAGS) +ber +noobj

%.erl:$(top_srcdir)/asn_src/ITU/%.asn
	$(ERLC) $(ASNCFLAGS) $<

TR.erl:$(top_srcdir)/asn_src/ITU/TR.asn \
		$(top_srcdir)/asn_src/ITU/TCAPMessages.asn
	$(ERLC) $(ASNCFLAGS) $<

TCAPMessages.erl:$(top_srcdir)/asn_src/ITU/TCAPMessages.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Useful-Definitions.asn
	$(ERLC) $(ASNCFLAGS) $<
	sed -i "/^-record('EXTERNAL'/,+6d" TCAPMessages.hrl

UnidialoguePDUs.erl:$(top_srcdir)/asn_src/ITU/UnidialoguePDUs.asn
	$(ERLC) $(ASNCFLAGS) $<
	sed -i "/^-record('EXTERNAL'/,+6d" UnidialoguePDUs.hrl

TC-Testing-User.erl:$(top_srcdir)/asn_src/ITU/TC-Testing-User.asn \
		$(top_srcdir)/asn_src/ITU/TC-TMP.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Information-Objects.asn
	$(ERLC) $(ASNCFLAGS) $<

TCAP-Examples.erl:$(top_srcdir)/asn_src/ITU/TCAP-Examples.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Information-Objects.asn
	$(ERLC) $(ASNCFLAGS) $<

TCAP-Tools.erl:$(top_srcdir)/asn_src/ITU/TCAP-Tools.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Information-Objects.asn
	$(ERLC) $(ASNCFLAGS) $<

TC-Notation-Extensions.erl:$(top_srcdir)/asn_src/ITU/TC-Notation-Extensions.asn \
		$(top_srcdir)/asn_src/ITU/TCAPMessages.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Useful-Definitions.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Information-Objects.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Generic-ROS-PDUs.asn \
		$(top_srcdir)/asn_src/ITU/UnidialoguePDUs.asn \
		$(top_srcdir)/asn_src/ITU/DialoguePDUsasn.
	$(ERLC) $(ASNCFLAGS) $<

Remote-Operations-Useful-Definitions.erl:$(top_srcdir)/asn_src/ITU/Remote-Operations-Useful-Definitions.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Information-Objects.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Generic-ROS-PDUs.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Useful-Definitions.asn
	$(ERLC) $(ASNCFLAGS) $<

Remote-Operations-Information-Objects.erl:$(top_srcdir)/asn_src/ITU/Remote-Operations-Information-Objects.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Information-Objects.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Generic-ROS-PDUs.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Useful-Definitions.asn
	$(ERLC) $(ASNCFLAGS) $<

CLEANFILES = $(ASN_MODULES) \
		$(patsubst %.erl,%.asn1db,$(ASN_MODULES)) \
		$(patsubst %.erl,%.hrl,$(ASN_MODULES))
