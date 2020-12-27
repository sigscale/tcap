
ROSEINCLUDES	= -I ../itu

EBIN			= ../../ebin/ansi

ERLC			= erlc
ERLCFLAGS	= -W -v +warn_unused_vars +debug_info -o $(EBIN)

ASNC			= $(ERLC) 
ASNCFLAGS	= -W +ber +noobj +debug_info $(ROSEINCLUDES)

$(EBIN)/%.beam:%.erl
	$(ERLC) $(ERLCFLAGS) $<

.SECONDARY:	%.erl
%.erl:%.asn
	$(ASNC) $(ASNCFLAGS) $<

.PHONY:	default
default:	all

.PHONY:	all
all:	$(EBIN)/TR.beam 
#		$(EBIN)/TCAPPackage.beam \
#		$(EBIN)/TCAP-Remote-Operations-Information-Objects.beam

TCAPPackage.erl:	$(EBIN)/TCAP-Remote-Operations-Information-Objects.beam


.PHONY:	clean
clean:
	- rm -f *.asn1db 
	- rm -f $(EBIN)/TR.beam
#			$(EBIN)/TCAPPackage.beam  \
#			$(EBIN)/TCAP-Remote-Operations-Information-Objects.beam
	- rm -f TR.erl TCAPPackage.erl
	- rm -f TR.hrl TCAPPackage.hrl

