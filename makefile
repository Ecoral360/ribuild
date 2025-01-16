.DELETE_ON_ERROR:
.PHONY: all clean

RSC := bin/rsc

all: ribuild

ribuild bin/rib: ribbit
	$(RSC) -t js -f+ v-port -o out.js ./src/rb.scm 
	node out.js build
	rm out.js

ribbit:
	git clone https://github.com/Ecoral360/ribbit.git
	cd ribbit
	$(MAKE) rsc.exe

install: bin/rib
	@echo "Add executables (rib and rsc) to your .basrc by running this command:"
	@echo "echo 'export PATH=\"\$$PATH\":\"`pwd`/bin\"' >> ~/.bashrc"

clean:
	rm -fr ribbit
	rm bin/rsc
	rm bin/rib

