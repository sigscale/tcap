## $Id: Makefile,v 1.5 2005/02/10 05:20:17 vances Exp $

EBIN			= ../ebin

ERL			= erl
ERLC			= erlc
ERLCFLAGS	= -o $(EBIN) -W -v +warn_unused_vars -I../include

$(EBIN)/%.beam:%.erl
	$(ERLC) $(ERLCFLAGS) $<

BEAMS		=	$(EBIN)/tcap.beam $(EBIN)/tcap_app.beam \
						$(EBIN)/tcap_sup.beam \
						$(EBIN)/tcap_sap_sup.beam \
						$(EBIN)/tcap_transaction_sup.beam \
						$(EBIN)/tcap_dialogue_sup.beam \
						$(EBIN)/tcap_components_sup.beam \
						$(EBIN)/tcap_invocation_sup.beam \
						$(EBIN)/tcap_user.beam

.PHONY:	default
default:	all

.PHONY:	all
all:	$(BEAMS) itu ansi

.PHONY:	itu
itu:
	cd ITU && $(MAKE)

.PHONY:	ansi
ansi:
	cd ANSI && $(MAKE)

.PHONY:	clean
clean:
	- rm -f $(BEAMS)
	cd ITU && $(MAKE) $@
	cd ANSI && $(MAKE) $@

