all: vcmieditor

vcmieditor:
	lazbuild 3rd_party/csvdocument/csvdocument_package.lpk vcmieditor.lpr

build_tests: vcmieditor tests/tests.lpi tests/tests.lpr
	lazbuild tests/tests.lpr

test: build_tests
	./tests/tests --all --progress --format=plain

clean_tests:
	rm -rf tests/tests tests/*.o tests/*.ppu tests/*.compiled

clean: clean_tests
	rm -rf vcmieditor *.o *.ppu *.compiled 3rd_party/csvdocument/lib/
