all: vcmieditor

vcmieditor:
	lazbuild 3rd_party/opengl/vcmiopenglcontext.lpk vcmieditor.lpr

build_tests:
	lazbuild tests/unit_tests.lpr

test: build_tests
	./tests/unit_tests --all --progress --format=plain

clean_tests:
	rm -rf tests/unit_tests tests/*.o tests/*.ppu tests/*.compiled

clean: clean_tests
	rm -rf vcmieditor *.o *.ppu *.compiled
