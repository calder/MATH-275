PS=7

F=PS$(PS)

all:
	@r --vanilla < $F.R
	@pdflatex $F.tex
	@rm -f *.aux *.log *.synctex.gz

o: all
	@open $F.pdf

clean:
	@rm -f *.aux *.log *.synctex.gz *.pdf