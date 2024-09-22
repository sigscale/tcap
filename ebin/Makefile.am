## Makefile.am
##
## Process this file with automake to produce Makefile.in

SUBDIRS = itu # ansi

appdir = $(beamdir)
app_DATA = tcap.app tcap.appup
EXTRA_DIST = tcap.app tcap.appup

beam_DATA = tcap.beam \
		tcap_app.beam \
		tcap_sup.beam \
		tcap_tsl_sup.beam \
		tcap_transaction_sup.beam \
		tcap_csl_sup.beam \
		tcap_dialogue_sup.beam \
		tcap_components_sup.beam \
		tcap_invocation_sup.beam \
		tcap_tco_callback.beam

%.beam:$(top_srcdir)/src/%.erl	$(top_srcdir)/include/tcap.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include -I $(top_builddir)/src/ITU $<

%.app:   %.app-in
	sed -e 's/%APP_NAME%/$(PACKAGE_NAME)/' \
			-e 's/%APP_VERSION%/$(PACKAGE_VERSION)/' < $< > $@

CLEANFILES = $(beam_DATA) $(app_DATA)

