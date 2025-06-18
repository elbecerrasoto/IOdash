.PHONY all:
all:
	Rscript populate

.PHONY help:
help:
	less Makefile

.PHONY check:
check:
	make clean
	make all 

.PHONY style:
style:
	Rscript -e 'styler::style_dir(".", recursive = FALSE)'

.PHONY clean:
clean:
	rm -f data


