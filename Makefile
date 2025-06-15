.PHONY help:
help:
	less Makefile

.PHONY style:
style:
	Rscript -e 'styler::style_dir(".", recursive = FALSE)'

mip_br.tsv:
	Rscript download.R 2> /dev/null


