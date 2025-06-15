XLSX = mip_ixi_br_sin_d_2018.xlsx
STEM = https://www.inegi.org.mx/contenidos/investigacion/coumip/tabulados
URL = $(STEM)/$(XLSX)

.PHONY help:
help:
	less Makefile

.PHONY check:
check:
	make clean
	make mip_br.tsv

.PHONY style:
style:
	Rscript -e 'styler::style_dir(".", recursive = FALSE)'

$(XLSX):
	wget $(URL)

mip_br.tsv: $(XLSX)
	Rscript mip_br.R $(XLSX) 2> /dev/null

.PHONY clean:
clean:
	rm -f mip_br.tsv $(XLSX) 


