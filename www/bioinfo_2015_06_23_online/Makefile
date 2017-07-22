all:
	Rscript -e 'knitr::knit("weiner_bioinfo_2015_06_23.Rmd")'
	pandoc -s -S -t revealjs weiner_bioinfo_2015_06_23.md -o weiner_bioinfo_2015_06_23.html \
		--mathjax='http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' \
		--css css/mytheme.css   --slide-level 2 -V theme=blood 

archive: all pdf
	mkdir -p bioinfo_2015_06_23_online
	cp -a Makefile reveal.js/ weiner_bioinfo_2015_06_23.html weiner_bioinfo_2015_06_23.pdf weiner_bioinfo_2015_06_23.Rmd img/ figure/ css/ bioinfo_2015_06_23_online/
	tar czf bioinfo_2015_06_23_online.tar.gz bioinfo_2015_06_23_online

pdf: all
	pandoc -s -S -o weiner_bioinfo_2015_06_23_ugly.pdf weiner_bioinfo_2015_06_23.md
	pandoc -s -S -t revealjs weiner_bioinfo_2015_06_23.md -o weiner_bioinfo_2015_06_23_wb.html \
		--mathjax='http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' \
		--css css/mytheme.css --css css/white.css  \
		--slide-level 2 -V theme=serif 


shiny: archive all
	cp -a bioinfo_2015_06_23_online bioinfo_2015_06_23_online.tar.gz ../../shiny/www
