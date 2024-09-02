

.PHONY: server all elm js site vscode http

all: js elm
	
elm:
	elm make src/Main.elm --output=js/elm.js

js:
	npx esbuild --outfile=js/bundle.js ts/bundle.ts --bundle --global-name=Bundle --format=iife

# run http server
http:
	npx esbuild --servedir=.

server: server.js
	node server.js

server.js: ts/interface.d.ts server.ts
	npx tsc $^




