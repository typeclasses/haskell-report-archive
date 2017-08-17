# Makefile for the Haskell Report
# read the README file before you start!
#
PARTS =		preface-11.tex preface-10.tex intro.tex lexemes.tex  exps.tex \
		decls.tex  modules.tex  basic.tex  io.tex \
		prelude.tex syntax.tex syntax-changes-11.tex \
		iosemant.tex iooptions.tex  derived.tex \
		index-extra.tex index-intro.tex layout.tex
PRELUDE =	Prelude.tex PreludeBuiltin.tex PreludeCore.tex \
		PreludeRatio.tex PreludeComplex.tex \
		PreludeList.tex PreludeArray.tex PreludeText.tex \
		PreludeIO.tex
#		static.tex short_semantics.tex 

RM = rm -f
#LATEX=olatex
LATEX=latex

# splitAndIndexPgm won't work unless you have "perl"
SPLITPGM = splitAndIndexPgm

.SUFFIXES:	.hi .hs .verb .tex .dvi

.verb.tex:
	expand < $< | ./verbatim >$@

.hs.tex:
	expand < $< | $(SPLITPGM) | ./verbatim >$@

.hi.tex:
	expand < $< | $(SPLITPGM) | ./verbatim >$@

.tex.dvi:
	$(LATEX) $<

haskell.dvi: verbatim haskell.tex haskell.ind class-fig.tex ${PARTS} ${PRELUDE}

# remove this rule if you don't have "makeindex"
haskell.ind: haskell.idx
	sed -f fixIndex < haskell.idx |\
	makeindex -i -t haskell.ilg > haskell.ind

verbatim: verbatim.lex
	lex verbatim.lex
	$(CC) lex.yy.c -ll -o verbatim
	$(RM) lex.yy.c

clean:
	$(RM) verbatim *.dvi *.log *.aux *.ilg *.blg *.toc ${PARTS} haskell.tex
	cp haskell.ind haskell.ind.two-sided
	echo "Don't delete the Prelude*.tex files"
	echo "Not everyone has \"perl\" to re-make them"
