DATA = data
TARGZ = .data.tar.gz
ENTRIES = $(DATA)/entries.tsv

.PHONY help:
help:
	less Makefile

$(DATA): $(TARGZ)
	@if [ -d "$(DATA)" ]; then echo "$(DATA) already exists, skipping extraction."; else tar -xzvf $< ; fi
	@touch $@ # Update data/ creation, as tar -xz keep the tape date.

.PHONY style:
style:
	Rscript -e 'styler::style_dir(".", recursive = FALSE)'


.PHONY blob-size:
blob-size:
	@git rev-list --objects --all | git cat-file --batch-check='%(objecttype) %(objectsize) %(rest)' | perl -ne 'print if !/^commit|^tree/' | sort -nrk2 | perl -ane '$$KB = int($$F[1]/1000); print "$$F[0] $${KB}KB $$F[2]\n"'


$(ENTRIES): $(DATA)
	curl https://ftp.ebi.ac.uk/pub/databases/interpro/current_release/entry.list > $(ENTRIES)

.PHONY clean:
	rm -rf results data
