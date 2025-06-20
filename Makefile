.PHONY help:
help:
	less Makefile

.PHONY style:
style:
	Rscript -e 'styler::style_dir(".", recursive = FALSE)'


