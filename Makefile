

.PHONY: server all

all: watcher.js
	elm make src/Main.elm --output=elm.js

server: server.js
	node server.js

server.js: server.ts
	npx tsc interface.d.ts $<

watcher.js: watcher.ts
	npx tsc interface.d.ts $<


