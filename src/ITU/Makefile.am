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
		TC-Notation-Extensions.erl \
		TC-TMP.erl \
		TC-Testing-User.erl \
		TCAP-Examples.erl \
		TCAP-Tools.erl

erl_DATA = tcap_tco_server.erl \
		tcap_cco_server.erl \
		tcap_dha_fsm.erl \
		tcap_tsm_fsm.erl \
		tcap_ism_fsm.erl \
		$(ASN_MODULES)

einclude_DATA = TR.hrl \
		TC.hrl \
		TCAPMessages.hrl \
		DialoguePDUs.hrl \
		UnidialoguePDUs.hrl \
		Remote-Operations-Generic-ROS-PDUs.hrl \
		TCAP-Examples.hrl \
		TC-Testing-User.hrl \
		TC-TMP.hrl

ASNCFLAGS = $(ERLCFLAGS) +ber +noobj

%.erl %.hrl: $(top_srcdir)/asn_src/ITU/%.asn
	$(ERLC) $(ASNCFLAGS) $<

TR.erl TR.hrl: $(top_srcdir)/asn_src/ITU/TR.asn \
		$(top_srcdir)/asn_src/ITU/TCAPMessages.asn
	$(ERLC) $(ASNCFLAGS) $<

TCAPMessages.erl TCAPMessages.hrl: \
		$(top_srcdir)/asn_src/ITU/TCAPMessages.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Useful-Definitions.asn
	$(ERLC) $(ASNCFLAGS) $<
	sed -e "/^-record('EXTERNAL'/,/^})\./d" TCAPMessages.hrl > TCAPMessages.hrl.tmp
	mv TCAPMessages.hrl.tmp TCAPMessages.hrl

UnidialoguePDUs.erl UnidialoguePDUs.hrl: \
		$(top_srcdir)/asn_src/ITU/UnidialoguePDUs.asn
	$(ERLC) $(ASNCFLAGS) $<
	sed -e "/^-record('EXTERNAL'/,/^})\./d" UnidialoguePDUs.hrl > UnidialoguePDUs.hrl.tmp
	mv UnidialoguePDUs.hrl.tmp UnidialoguePDUs.hrl

TC-Testing-User.erl TC-Testing-User.hrl: \
		$(top_srcdir)/asn_src/ITU/TC-Testing-User.asn \
		$(top_srcdir)/asn_src/ITU/TC-TMP.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Information-Objects.asn
	$(ERLC) $(ASNCFLAGS) $<

TCAP-Examples.erl TCAP-Examples.hrl: \
		$(top_srcdir)/asn_src/ITU/TCAP-Examples.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Information-Objects.asn
	$(ERLC) $(ASNCFLAGS) $<

TCAP-Tools.erl TCAP-Tools.hrl: \
		$(top_srcdir)/asn_src/ITU/TCAP-Tools.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Information-Objects.asn
	$(ERLC) $(ASNCFLAGS) $<

TC-Notation-Extensions.erl TC-Notation-Extensions.hrl: \
		$(top_srcdir)/asn_src/ITU/TC-Notation-Extensions.asn \
		$(top_srcdir)/asn_src/ITU/TCAPMessages.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Useful-Definitions.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Information-Objects.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Generic-ROS-PDUs.asn \
		$(top_srcdir)/asn_src/ITU/UnidialoguePDUs.asn \
		$(top_srcdir)/asn_src/ITU/DialoguePDUs.asn
	$(ERLC) $(ASNCFLAGS) $<

Remote-Operations-Useful-Definitions.erl \
		Remote-Operations-Useful-Definitions.hrl: \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Useful-Definitions.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Information-Objects.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Generic-ROS-PDUs.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Useful-Definitions.asn
	$(ERLC) $(ASNCFLAGS) $<

Remote-Operations-Information-Objects.erl \
		Remote-Operations-Information-Objects.hrl: \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Information-Objects.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Information-Objects.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Generic-ROS-PDUs.asn \
		$(top_srcdir)/asn_src/ITU/Remote-Operations-Useful-Definitions.asn
	$(ERLC) $(ASNCFLAGS) $<

CLEANFILES = $(ASN_MODULES) \
		$(patsubst %.erl,%.asn1db,$(ASN_MODULES)) \
		$(patsubst %.erl,%.hrl,$(ASN_MODULES))

