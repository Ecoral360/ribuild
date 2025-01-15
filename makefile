all: 
	@rsc -t js -f+ v-port -o out.js ./src/rb.scm && node out.js build
