Examples:

- standalone.tex: this is an example of how to use the standalone latex class to render a big diagram. The point of the standalone class is that there is everything is rendered on a single pdf page which is as big as necessary.

coreact-yade standalone.tex --external-output

(the external output option means that the latex code generated from the diagram is not inlined in standalone.tex put in an external late, included with \input)

- lyx/, tex/

Open those directories with the web app will make it watch the files example.lyx (resp. example.tex)



