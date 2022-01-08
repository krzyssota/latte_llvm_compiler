GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc
SRC=src
#BNFC=/home/students/inf/PUBLIC/MRJP/bin/bnfc
BNFC=bnfc

.PHONY : all clean distclean

all: bnfc RunStaticAnalysis

bnfc: $(SRC)/Latte.cf
	cd $(SRC) && ${BNFC} -m --functor -haskell Latte.cf && make && cd ..

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

runtime:
	clang -O0 -o lib/runtime.ll -emit-llvm -S lib/runtime.c

RunStaticAnalysis: $(SRC)/RunStaticAnalysis.hs $(SRC)/StaticAnalysis.hs $(SRC)/AbsLatte.hs $(SRC)/LexLatte.hs $(SRC)/ParLatte.hs $(SRC)/PrintLatte.hs
	cd $(SRC) && ${GHC} ${GHC_OPTS} $@ && mv RunStaticAnalysis latc

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi

distclean : clean
	-rm -f AbsLatte.hs AbsLatte.hs.bak ComposOp.hs ComposOp.hs.bak DocLatte.txt DocLatte.txt.bak ErrM.hs ErrM.hs.bak LayoutLatte.hs LayoutLatte.hs.bak LexLatte.x LexLatte.x.bak ParLatte.y ParLatte.y.bak PrintLatte.hs PrintLatte.hs.bak SkelLatte.hs SkelLatte.hs.bak TestLatte.hs TestLatte.hs.bak XMLLatte.hs XMLLatte.hs.bak ASTLatte.agda ASTLatte.agda.bak ParserLatte.agda ParserLatte.agda.bak IOLib.agda IOLib.agda.bak Main.agda Main.agda.bak Latte.dtd Latte.dtd.bak TestLatte LexLatte.hs ParLatte.hs ParLatte.info ParDataLatte.hs Makefile

