Run yade with --help to see a description.

Examples:
- example.lyx

yade --watch example.lyx --external-tex --dir diagrams --include-cmd "filename \"@\"" --prefix "\end_layout\n\end_inset\n\begin_inset Preview \n\begin_layout Standard \n\begin_inset CommandInset include\nLatexCommand input\npreview true" --suffix "\end_inset\n\end_layout\n\end_inset\n\begin_inset Note Note\nstatus open\n\begin_layout Plain Layout" 

- example.tex 

yade --watch example.tex
