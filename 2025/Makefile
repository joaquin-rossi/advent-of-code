.PHONY: all build clean

all: build

build: build/aoc_01

clean:
	rm -rf build

ADAFLAGS = -gnat2022 -O2
LDFLAGS =

build/aoc_01: src/aoc_01.adb
	@mkdir -p $(dir $@)
	gnatmake -D build $^ -o $@ $(ADAFLAGS) $(LDFLAGS)
