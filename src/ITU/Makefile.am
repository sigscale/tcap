## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

erl_DATA = TR.erl \
		TC.erl \
		TCAPMessages.erl \
		DialoguePDUs.erl \
		UnidialoguePDUs.erl \
		Remote-Operations-Generic-ROS-PDUs.erl \
		Remote-Operations-Information-Objects.erl \
		Remote-Operations-Useful-Definitions.erl \
		TC-TMP.erl \
		TC-Testing-User.erl \
		TCAP-Examples.erl \
		TCAP-Tools.erl

%.erl:$(top_srcdir)/asn_src/ITU/%.asn
	$(ERLC) $(ERLCFLAGS) -b ber +noobj $<

CLEANFILES = $(erl_DATA) \
	TR.asn1db \
	TC.asn1db \
	DialoguePDUs.asn1db \
	UnidialoguePDUs.asn1db \
	Remote-Operations-Generic-ROS-PDUs.asn1db \
	Remote-Operations-Information-Objects.asn1db \
	Remote-Operations-Useful-Definitions.asn1db \
	TC-TMP.asn1db \
	TC-Testing-User.asn1db \
	TCAP-Examples.asn1db \
	TCAP-Tools.asn1db \
	TR.hrl \
	TC.hrl \
	DialoguePDUs.hrl \
	UnidialoguePDUs.hrl \
	Remote-Operations-Generic-ROS-PDUs.hrl \
	Remote-Operations-Information-Objects.hrl \
	Remote-Operations-Useful-Definitions.hrl \
	TC-TMP.hrl \
	TC-Testing-User.hrl \
	TCAP-Examples.hrl \
	TCAP-Tools.hrl

