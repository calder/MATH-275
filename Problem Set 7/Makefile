PS=7

F=PS$(PS)

all:
	@r --vanilla < $F.R
	@pdflatex $F.tex
	@rm *.aux *.log

o: all
	@open $F.pdf

clean:
	@rm -f *.aux *.log *.pdf