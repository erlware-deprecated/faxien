# ----------------------------------------------------
# Make file for creating a release. 
# ----------------------------------------------------

##
# Basename of this release.
##
RELS=$(shell basename `pwd`)

include ../../build/otp.mk

include ./vsn.mk

USR_LIBPATH=../../lib
ERLWARE_LIB_PATH=$(ERLWARE_HOME)/application_packages/$(TARGET_ERTS_VSN)/lib
FSLIB_DIR=$(ERLWARE_LIB_PATH)/$(shell ls $(ERLWARE_LIB_PATH) | grep fslib | sort -r | head -n 1)
INSTALL_DIR=/usr/local/lib
ABS_USER_LIBPATH=$(shell cd ../../lib;pwd)
ERL=$(shell ERL=$(which erl); if [ "$${ERL}" = "" ];then ERL=$(ERLWARE_HOME)/bin/erl; fi; echo $${ERL})
FAXIEN=$(shell FAXIEN=$(which faxien); if [ "$${FAXIEN}" = "" ];then FAXIEN=$(ERLWARE_HOME)/bin/faxien; fi; echo $${FAXIEN})


USER_DEFINED_PACKAGE_FILES=control install.sh

# ----------------------------------------------------
# CREATE DIR STRUCTURE HERE
# ----------------------------------------------------

BUILD_FILES=fs_boot_smithe.beam fs_lists.beam fs_lib.beam

LOCAL_DIR=local
PACKAGE_DIR=$(RELS)-$(REL_VSN)

DIR_STRUCTURE= \
	$(LOCAL_DIR) \
	$(LOCAL_DIR)/log/$(REL_VSN) \

RELEASE_DEFINITION_FILES= \
	$(RELS).rel \
	$(RELS).script \
	$(RELS).boot

LOCAL_RELEASE_DEFINITION_FILES= \
	$(LOCAL_DIR)/$(RELS).script \
	$(LOCAL_DIR)/$(RELS).boot

LOCAL_TARGET_FILES=$(LOCAL_HTTP_CONF) $(LOCAL_DIR)/$(RELS).config $(LOCAL_RELEASE_DEFINITION_FILES)

LOCAL_TARGETS=$(DIR_STRUCTURE) $(LOCAL_DIR)/$(RELS).sh vsnit $(LOCAL_TARGET_FILES) 

# ----------------------------------------------------
# TARGETS
# ----------------------------------------------------

all debug opt instr script: $(LOCAL_TARGETS) 

package: $(PACKAGE_DIR) 

$(DIR_STRUCTURE):
	mkdir -p $@

clean:
	$(RM) -r $(PACKAGE_DIR)
	$(RM) $(REL_SCRIPTS) $(TARGET_FILES)
	$(RM) -r $(LOCAL_DIR) $(PRODN_DIR)
	$(RM) $(RELS).rel
	$(RM) -r $(RELS)
	$(RM) $(RELS)*.tgz
	$(RM) $(RELS).rel.src.tmp
	$(RM) $(RELEASE_DEFINITION_FILES)
	
# ----------------------------------------------------
# TARGETS FOR LOCAL MODE
# ----------------------------------------------------

# startup script for local mode
$(LOCAL_DIR)/$(RELS).sh: 
	@echo '#!/bin/sh' > $@
	@echo "cd $(CURDIR)/$(LOCAL_DIR)" >> $@
	@echo "$(ERL) -name $${USER}_$(RELS) -boot $(RELS) -config $(RELS).config \$$@" >> $@
	chmod +x $@
	@echo
	@echo "==== Start local node with \"sh $@\" ===="
	@echo

# Create the config file for local mode.
$(LOCAL_DIR)/$(RELS).config: $(RELS).config.src
	sed -e 's;%LOG_OTP%;$(CURDIR)/$(LOCAL_DIR)/log/$(REL_VSN);' \
            -e 's;%REL_NAME%;$(RELS);g' \
            -e 's;%HOME%;$(HOME);g' \
            -e 's;%BROADCAST_ADDRESS%;$(BROADCAST_ADDRESS);g' \
            -e 's;%CONTACT_NODE%;$(CONTACT_NODE);g' \
	    -e "s;%HOSTNAME%;`hostname --long`;" \
             $< > $@

# Create the release file for local mode.
vsnit: $(RELS).rel.src 
	sed -e 's;%REL_VSN%;$(REL_VSN);' \
	    -e 's;%REL_NAME%;$(RELS);' \
	    -e 's;%ERTS_VSN%;$(TARGET_ERTS_VSN);' \
	       $< > $<.tmp

# Create and position script and boot files for local mode.
$(LOCAL_RELEASE_DEFINITION_FILES): 
	@echo "FSLIB_DIR: $(FSLIB_DIR)"
	@ $(ERL) -pz $(FSLIB_DIR)/ebin \
	      -noshell \
	      -s fs_lib commandline_apply fs_boot_smithe make_script_and_boot [\"$(ERLWARE_LIB_PATH)/*\",\"$(ERL_RUN_TOP)/*\",\"$(USR_LIBPATH)\"] \
                \"$$(basename `pwd`).rel.src.tmp\" \
                [local] \
	      -s init stop
	mv $(RELEASE_DEFINITION_FILES) $(LOCAL_DIR)/ 

# ----------------------------------------------------
# TARGETS FOR PACKAGE CREATION
# ----------------------------------------------------

# Create the config file for a package.
$(PACKAGE_DIR)/releases/$(REL_VSN)/$(RELS).config: $(RELS).config.src
	mkdir -p `dirname $@`
	sed -e 's;%LOG_OTP%;/tmp/;' \
            -e 's;%REL_NAME%;$(RELS);g' \
            -e 's;%REL_VSN%;$(REL_VSN);g' \
            -e 's;%HOME%;$(HOME);g' \
            -e 's;%BROADCAST_ADDRESS%;$(BROADCAST_ADDRESS);g' \
            -e 's;%CONTACT_NODE%;$(CONTACT_NODE);g' \
             $< > $@

# Create the executable script
$(PACKAGE_DIR)/bin/$(RELS): $(RELS).src
	mkdir -p $(PACKAGE_DIR)/bin
	sed -e 's;%REL_VSN%;$(REL_VSN);g' \
	    -e "s;%TARGET_ERTS_VSN%;$(TARGET_ERTS_VSN);" \
	    -e "s;%INVOCATION_SUFFIX%;-detached;" \
             $< > $@


$(PACKAGE_DIR): $(LOCAL_TARGETS) $(PACKAGE_DIR)/bin/$(RELS) $(PACKAGE_DIR)/releases/$(REL_VSN)/$(RELS).config 
	cp $(USER_DEFINED_PACKAGE_FILES) $(PACKAGE_DIR)/
	cp $(LOCAL_DIR)/$(RELS).rel $@/releases/$(REL_VSN)
	@echo "FSLIB_DIR: $(FSLIB_DIR)"
	rm -rf $@/lib
	mkdir $@/lib
	@ $(ERL) -pz $(FSLIB_DIR)/ebin \
	      -noshell \
	      -s fs_lib commandline_apply fs_boot_smithe stage_apps [\"$(ERLWARE_LIB_PATH)/*\",\"$(USR_LIBPATH)/*\"] \
                \"$(LOCAL_DIR)/$(RELS).rel\" \
                $@/lib \
	      -s init stop

# ----------------------------------------------------
# TARGETS FOR INSTALLATION
# ----------------------------------------------------

install: $(PACKAGE_DIR)
	$(FAXIEN) install $(PACKAGE_DIR)
