## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

modhtml = tcap.html \
		tcap_app.html \
		tcap_sup.html \
		tcap_sap_sup.html \
		tcap_transaction_sup.html \
		tcap_dialogue_sup.html \
		tcap_components_sup.html \
		tcap_invocation_sup.html \
		tcap_user.html \
		tcap_cco_server.html \
		tcap_dha_fsm.html \
		tcap_ism_fsm.html \
		tcap_tco_server.html \
		tcap_tsm_fsm.html

doc_DATA = overview.edoc $(modhtml) \
		modules-frame.html overview-summary.html \
		index.html stylesheet.css \
		sigscale-logo.png supervision.png \
		ae_invocations.png \
		application_process.png \
		inap_aei.png \
		osi_model.png \
		ssn_distribution.png \
		tcap_application_process.png \
		tcap_distribution.png \
		tcap_messaging.png \
		tcap_supervision.png \
		transaction_capabilities.png

overview.edoc: overview.edoc-in
	sed -e 's!%VERSION%!$(VERSION)!' \
			-e 's!%DESCRIPTION%!$(PACKAGE)!' < $< > $@

%.html:$(top_srcdir)/src/%.erl	overview.edoc $(top_srcdir)/doc/supervision.edoc
	$(ERL) -noshell -run edoc_run application \
			"'$(PACKAGE_NAME)'" "'$(top_builddir)'" \
			'[{sort_functions,false},{private,true},{todo,true},{source_path,["$(top_srcdir)/src"]},{subpackages,false},{def,{docsrc,"$(top_srcdir)/doc/{@?}"}},{stylesheet_file, "$(top_srcdir)/doc/sigscale.css"}]'
	for i in `ls *.html`; do \
			sed -e 's!erlang logo!sigscale logo!' \
					-e 's!href=\"http://www.erlang.org/\"!href=\"http://www.sigscale.org/\"!' \
					-e 's!erlang.png!sigscale-logo.png!' $$i > $$i.tmp; \
			mv $$i.tmp $$i; \
	done
	-cp -n $(top_srcdir)/doc/sigscale-logo.png .
	-cp -n $(top_srcdir)/doc/supervision.png .

CLEANFILES = overview.edoc edoc-info index.html \
		modules-frame.html overview-summary.html packages-frame.html \
		stylesheet.css erlang.png $(modhtml)

