XLSX = mip_ixi_br_sin_d_2018.xlsx
STEM = https://www.inegi.org.mx/contenidos/investigacion/coumip/tabulados
URL = $(STEM)/$(XLSX)

TSV = mip_br.tsv

.PHONY all:
all: $(TSV)

.PHONY help:
help:
	less Makefile

.PHONY check:
check:
	make clean
	make $(TSV)

.PHONY style:
style:
	Rscript -e 'styler::style_dir(".", recursive = FALSE)'

$(XLSX):
	wget $(URL)

$(TSV): $(XLSX)
	Rscript clean.R $(XLSX) $(TSV) 2> /dev/null

.PHONY clean:
clean:
	rm -f $(XLSX) $(TSV)


