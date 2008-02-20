SIDES = lib release

all clean package install:
	@for dir in $(SIDES); do \
		(cd $$dir; ${MAKE} $@); \
		if [ "$$?" -ne "0" ]; then ERROR=$$?; echo "Error Code $$ERROR"; exit $$ERROR; fi; \
	done

docs:
	(cd lib; ${MAKE} $@); \
	if [ "$$?" -ne "0" ]; then ERROR=$$?; echo "Error Code $$ERROR"; exit $$ERROR; fi; 
