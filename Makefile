nea.pdf: nea.tex nea.bib final_files.tex final_files.zip final_files.tar.gz
	tectonic -Z shell-escape nea.tex

lualatex: build final_files.tex final_files.zip final_files.tar.gz
	mkdir -p build
	latexmk -shell-escape -pdf -lualatex -output-directory=build nea.tex

final_files.tex:
	git ls-files -- prog  | grep -v '\.gitignore' | grep -v 'package-lock.json' | grep -v rust-toolchain | cut -d '/' -f 2- | sed 's/.*\.\(.*\)/\\subsection{&} \\inputminted{\1}{\/home\/nils\/Documents\/education\/CS-NEA\/prog\/&}/' | sed -E 's/\{(input|output)\}/{text}/' | sed 's/{ast}/{rs}/' | awk -F '}' '{gsub(/_/,"\\_",$$1)}1' OFS=} >final_files.tex

final_files.tar.gz:
	cd prog && git archive --format=tar HEAD $(git ls-files -- prog  | grep -v '\.gitignore' | grep -v 'package-lock.json' | grep -v rust-toolchain) | gzip > ../final_files.tar.gz

final_files.zip:
	cd prog && git archive --format=zip HEAD $(git ls-files -- prog  | grep -v '\.gitignore' | grep -v 'package-lock.json' | grep -v rust-toolchain) > ../final_files.zip
