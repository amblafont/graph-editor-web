

.PHONY: server all elm js site vscode http

all: js elm katex service-worker.js

katex:
	set -e
	curl -L -O "https://github.com/KaTeX/KaTeX/releases/download/v0.16.11/katex.zip"
	unzip katex.zip
	mv katex katex-temp
	mkdir katex
	mv katex-temp/katex.min.js katex/
	mv katex-temp/fonts fonts
	{ echo "const katexStyles = \`"; cat katex-temp/katex.min.css; echo "\`;";} > katex/katex.min.css.js
	rm katex.zip
	rm -rf katex-temp
	
elm:
	elm make src/Main.elm --output=js/elm.js

js:
	npx esbuild --outfile=js/bundle.js ts/bundle.ts --bundle --global-name=Bundle --format=iife

# run http server
http:
	npx esbuild --servedir=.

server: server.js
	node server.js

server.js: ts/interface.d.ts ts/server.ts
	npx tsc --outDir . $^

service-worker.js: swbuilder.js
	node swbuilder.js



