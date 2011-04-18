GHC	= ghc -O2 -fasm -fforce-recomp -i../src -prof

all: bench qc

bench:: Benchmark.hs MemBench.hs CBenchmark.o
	$(GHC) -fliberate-case-threshold=1000 --make Benchmark.hs CBenchmark.o -o $@
	./$@ 100

bench-prof: Benchmark.hs MemBench.hs CBenchmark.o
	$(GHC) -auto-all -rtsopts -fliberate-case-threshold=1000 --make Benchmark.hs CBenchmark.o -o $@
	./$@ 100 +RTS -p

CBenchmark.o: CBenchmark.c
	gcc -O3 -c $< -o $@

qc: Tests.hs
	$(GHC) --make $< -o $@
	./$@

clean:
	$(RM) *.o *.hi bench qc

.PHONY: clean bench bench-nb
