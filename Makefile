nea.pdf: nea.tex nea.bib final_files.tex final_files.zip final_files.tar.gz
	tectonic -Z shell-escape nea.tex

lualatex: build final_files.tex final_files.zip final_files.tar.gz
	latexmk -shell-escape -pdf -lualatex -output-directory=build nea.tex

build:
	mkdir build

final_files.tex:
	git ls-files -- prog  | grep -v '\.gitignore' | grep -v 'package-lock.json' | grep -v rust-toolchain | cut -d '/' -f 2- | sed 's/.*\.\(.*\)/\\subsubsection{&} \\inputminted{\1}{.\/prog\/&}/' | sed -E 's/\{(input|output|stdin)\}/{text}/' | sed 's/{ast}/{rs}/' | awk -F '}' '{gsub(/_/,"\\_",$$1)}1' OFS=} >final_files.tex

final_files.tar.gz:
	cd prog && git archive --format=tar HEAD $(git ls-files -- prog  | grep -v '\.gitignore' | grep -v 'package-lock.json' | grep -v rust-toolchain) | gzip > ../final_files.tar.gz

final_files.zip:
	cd prog && git archive --format=zip HEAD $(git ls-files -- prog  | grep -v '\.gitignore' | grep -v 'package-lock.json' | grep -v rust-toolchain) > ../final_files.zip

clean:
	rm final_files.tar.gz
	rm final_files.zip
	rm final_files.tex
