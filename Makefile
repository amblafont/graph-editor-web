
KATEX= katex/katex.min.js katex/katex.min.css.js fonts
.PHONY: server all elm fastelm debugelm js site vscode http service-worker

all: js elm $(KATEX) service-worker

$(KATEX):
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

fastelm:
	elm make --optimize src/Main.elm --output=js/elm.js

debugelm:
	elm make --debug src/Main.elm --output=js/elm.js

js:
	npx esbuild --outfile=js/bundle.js ts/bundle.ts --bundle --global-name=Bundle --format=iife

# run http server
http:
	npx esbuild --servedir=.

server: server.js
	node server.js

server.js: ts/interface.d.ts ts/server.ts
	npx tsc --outDir . --downlevelIteration  $^

service-worker:
	node swbuilder.js



site:
	cp service-worker.js index.html ~/site/graph-editor/
	cp js/*.js ~/site/graph-editor/js

vscode:
	cp index.html ~/yade/code-ext-yade/media/
	cp js/*.js ~/yade/code-ext-yade/media/js
	cd ~/yade/code-ext-yade/media; find . -type f > files.txt
	# cp katex/* ~/yade/code-ext-yade/media/katex/
	# cp -r fonts ~/yade/code-ext-yade/media/
	# cp server.js ts/server.ts ts/interface.d.ts ~/yade/code-ext-yade/src/
