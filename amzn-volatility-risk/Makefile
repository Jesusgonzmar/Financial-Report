.PHONY: all install analysis report clean help

help:
	@echo "make install   - install the required R packages"
	@echo "make analysis  - run the full R pipeline (tables, figures, CSV outputs)"
	@echo "make report    - compile report/main.pdf with latexmk"
	@echo "make all       - analysis + report"
	@echo "make clean     - remove LaTeX build files"

all: analysis report

install:
	Rscript R/install_packages.R

analysis:
	Rscript R/run_all.R

report:
	cd report && latexmk -pdf -interaction=nonstopmode -halt-on-error main.tex

clean:
	cd report && latexmk -C
