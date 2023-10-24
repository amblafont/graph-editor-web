all:
	elm make src/Main.elm --output=elm.js
	yarn compile
	yarn make
install:
	dpkg -i out/make/deb/x64/coreact-yade_*_amd64.deb
