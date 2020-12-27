## $id$

SCCPINCDIR = ../../../SCCP/itu/include

EBIN			= ../../ebin/itu

ERL			= erl
ERLC			= erlc
ERLCFLAGS	= -o ${EBIN} -W -v +warn_unused_vars -I ../../asn_src/ITU -I ../../include -I ${SCCPINCDIR}

${EBIN}/%.beam:%.erl
	${ERLC} ${ERLCFLAGS} $<

BEAMS		=	${EBIN}/tcap_tco_server.beam \
						${EBIN}/tcap_tsm_fsm.beam \
						${EBIN}/tcap_dha_fsm.beam \
						${EBIN}/tcap_cco_server.beam \
						${EBIN}/tcap_ism_fsm.beam

.PHONY:	default
default:	all

.PHONY:	all
all:	${BEAMS}

.PHONY:	install

.PHONY:	clean
clean:
	- rm -f ${BEAMS}

tcap_tco_server.beam:	tcap_tco_server.erl \
				../../asn_src/TCAPMessages.hrl ../../include/tcap.hrl \
				${SCCPINCDIR}/sccp.hrl
tcap_tsm_fsm.beam:	tcap_tsm_fsm.erl \
				../../asn_src/TCAPMessages.hrl ../../include/tcap.hrl \
				${SCCPINCDIR}/sccp.hrl

