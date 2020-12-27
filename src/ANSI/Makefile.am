## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

erl_DATA = TCAP-Remote-Operations-Information-Objects.erl \
		TR.erl \
		TCAPPackage.erl

%.erl:$(top_srcdir)/asn_src/ANSI/%.asn
	$(ERLC) $(ERLCFLAGS) -b ber +noobj \
	-I $(top_srcdir)/asn_src/ITU \
	-I $(top_builddir)/src/ITU $<

CLEANFILES = $(erl_DATA) \
		TCAP-Remote-Operations-Information-Objects.asn1db \
		TR.asn1db \
		TCAPPackage.asn1db \
		TCAP-Remote-Operations-Information-Objects.hrl \
		TR.hrl \
		TCAPPackage.hrl

