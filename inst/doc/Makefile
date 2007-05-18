cacheSweave.tex:	cacheSweave.Rnw
			export R_DEFAULT_PACKAGES=NULL
			R CMD Sweave $<
			perl -npi -e 's/^%\<\</\<\</' $@
			perl -npi -e 's/^%\@/\@/' $@

cacheSweave.pdf:	cacheSweave.tex
			texi2dvi --pdf --clean --quiet $<

