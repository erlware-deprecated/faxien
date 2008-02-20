EBIN=../ebin
EMULATOR=beam

ERLC_WFLAGS = -W
ERLC_EX=$(shell ERLC=$(which erlc); if [ "$${ERLC}" = "" ];then ERLC=$(ERLWARE_HOME)/bin/erlc; fi; echo $${ERLC})
ERLC = $(ERLC_EX) $(ERLC_WFLAGS) $(ERLC_FLAGS)

ERL_EX=$(shell ERL=$(which erl); if [ "$${ERL}" = "" ];then ERL=$(ERLWARE_HOME)/bin/erl; fi; echo $${ERL})
ERL = $(ERL_EX) -boot start_clean

ESRC = .

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

.erl.beam:
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(dir $@) $<

