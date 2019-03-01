TMP=tmp
FLAGS=-shell-escape

report.pdf: report.tex
	mkdir -p $(TMP)
	pdflatex $(FLAGS) --output-directory $(TMP) report.tex 
	pdflatex $(FLAGS) --output-directory $(TMP) report.tex 
	mv $(TMP)/report.pdf .

clean:
	rm -r $(TMP)
	rm report.pdf
