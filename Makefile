.PHONY all:
all:
	Rscript populate.R

.PHONY help:
help:
	less Makefile

.PHONY check:
check:
	make all 

.PHONY style:
style:
	Rscript -e 'styler::style_dir(".", recursive = FALSE)'


