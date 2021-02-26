## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

modhtml = tcap.html \
		tcap_app.html \
		tcap_sup.html \
		tcap_tsl_sup.html \
		tcap_transaction_sup.html \
		tcap_csl_sup.html \
		tcap_dialogue_sup.html \
		tcap_components_sup.html \
		tcap_invocation_sup.html \
		tcap_cco_server.html \
		tcap_dha_fsm.html \
		tcap_ism_fsm.html \
		tcap_tco_server.html \
		tcap_tco_callback.html \
		tcap_tsm_fsm.html

doc_DATA = overview.edoc $(modhtml) \
		modules-frame.html overview-summary.html \
		index.html stylesheet.css \
		sigscale-logo.png \
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
			'[{sort_functions,false},{private,true},{todo,true},{source_path,["$(top_srcdir)/src","$(top_srcdir)/src/ITU"]},{subpackages,false},{def,{docsrc,"$(top_srcdir)/doc/{@?}"}},{stylesheet_file, "$(top_srcdir)/doc/sigscale.css"}]'
	for i in `ls *.html`; do \
			sed -e 's!erlang logo!sigscale logo!' \
					-e 's!href=\"http://www.erlang.org/\"!href=\"http://www.sigscale.org/\"!' \
					-e 's!erlang.png!sigscale-logo.png!' $$i > $$i.tmp; \
			mv $$i.tmp $$i; \
	done
	-cp -n $(top_srcdir)/doc/sigscale-logo.png .
	-cp -n $(top_srcdir)/doc/tcap_supervision.png .
	-cp -n $(top_srcdir)/doc/ae_invocations.png .
	-cp -n $(top_srcdir)/doc/application_process.png .
	-cp -n $(top_srcdir)/doc/inap_aei.png .
	-cp -n $(top_srcdir)/doc/osi_model.png .
	-cp -n $(top_srcdir)/doc/ssn_distribution.png .
	-cp -n $(top_srcdir)/doc/tcap_application_process.png .
	-cp -n $(top_srcdir)/doc/tcap_distribution.png .
	-cp -n $(top_srcdir)/doc/tcap_messaging.png .
	-cp -n $(top_srcdir)/doc/tcap_supervision.png .
	-cp -n $(top_srcdir)/doc/transaction_capabilities.png .

CLEANFILES = $(doc_DATA) \
		edoc-info \
		index.html \
		modules-frame.html \
		overview-summary.html \
		packages-frame.html \
		stylesheet.css \
		erlang.png \
		$(modhtml)

