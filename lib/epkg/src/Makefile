# $Id: Makefile,v 1.3 2004/08/13 16:35:59 mlogan Exp $
#
include ../../../build/otp.mk
include ../../../build/colors.mk
include ../../../build/buildtargets.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------

include ../vsn.mk
APP_NAME=epkg
PFX=epkg
VSN=$(EPKG_VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

ERLWARE_LIB_PATH=$(ERLWARE_HOME)/application_packages/$(TARGET_ERTS_VSN)/lib
FSLIB_DIR=$(ERLWARE_LIB_PATH)/$(shell ls $(ERLWARE_LIB_PATH) | grep fslib | sort -r | head -n 1)

MODULES = $(shell ls *.erl | sed s/.erl//)
MODULES_COMMA = $(shell ls *.erl | sed s/\\.erl/,/)

HRL_FILES=
INTERNAL_HRL_FILES= $(APP_NAME).hrl
ERL_FILES= $(MODULES:%=%.erl)
DOC_FILES=$(ERL_FILES)

APP_FILE= $(APP_NAME).app
APPUP_FILE= $(APP_NAME).appup

APP_SRC= $(APP_FILE).src
APPUP_SRC= $(APPUP_FILE).src

APP_TARGET= $(EBIN)/$(APP_FILE)
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

BEAMS= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
TARGET_FILES= $(BEAMS) $(APP_TARGET) $(APPUP_TARGET)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ERL_FLAGS +=
ERL_COMPILE_FLAGS += -I../include $(shell cat $(APP_NAME).includes)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

all debug opt: $(APP_NAME).includes $(EBIN) $(TARGET_FILES) 

include ../../../build/docs.mk

# Note: In the open-source build clean must not destroy the preloaded
# beam files.
clean:
	rm -f $(TARGET_FILES)
	rm -f core
	rm -rf $(EBIN)
	rm -rf *html
	rm -f erl_crash.dump
	rm -f *~*
	rm -f $(APP_NAME).includes 

$(EBIN):
	mkdir $(EBIN)

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(APP_NAME).includes:
	$(ERL) -pz $(FSLIB_DIR)/ebin -noshell -s fs_lib commandline_apply fs_boot_smithe write_out_include_dirs \
	[\"$(ERLWARE_LIB_PATH)/*\",\"../../*\"] \"$(APP_NAME).app.src\" \"$@\" -s init stop 

$(APP_TARGET): $(APP_SRC) ../vsn.mk $(BEAMS)
	sed -e 's;%VSN%;$(VSN);' \
		-e 's;%PFX%;$(PFX);' \
		-e 's;%APP_NAME%;$(APP_NAME);' \
		-e 's;%MODULES%;%MODULES%$(MODULES_COMMA);' \
		$< > $<".tmp" 
	sed -e 's/%MODULES%\(.*\),/\1/' \
		$<".tmp" > $@ 
	rm $<".tmp"


$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

