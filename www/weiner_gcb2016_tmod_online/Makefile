ROOT=weiner_gcb2016_tmod

all: knit
	pandoc -s -S -t revealjs $(ROOT).md -o $(ROOT).html \
		--mathjax='http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' \
		--css css/mytheme.css   --slide-level 2 -V theme=blood 
	pandoc -s -S -t revealjs $(ROOT).md -o $(ROOT)_local.html \
		--mathjax='file:///home/january/.local/share/MathJax/MathJax.js?config=TeX-AMS-MML_HTMLorMML' \
		--css css/mytheme.css   --slide-level 2 -V theme=blood 

archive: all pdf 
	mkdir -p $(ROOT)_online
	cp -a Makefile reveal.js/ weiner_gcb2016_tmod.html $(ROOT).pdf $(ROOT).Rmd img/ figure/ css/ $(ROOT)_online/
	tar czf $(ROOT)_online.tar.gz $(ROOT)_online

pdf: all
	pandoc -s -S -o $(ROOT)_ugly.pdf $(ROOT).md
	pandoc -s -S -t revealjs $(ROOT).md -o $(ROOT)_wb.html \
		--mathjax='http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' \
		--css css/mytheme.css --css css/white.css  \
		--slide-level 2 -V theme=serif 

online: archive 
	cp -a $(ROOT)_online $(ROOT)_online.tar.gz ../../shiny/www

full: online all pdf

knit:
	Rscript -e 'knitr::knit("$(ROOT).Rmd")'


