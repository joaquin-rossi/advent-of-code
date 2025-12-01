.PHONY: all build clean

all: build

build: build/day_01

clean:
	rm -rf build

ADAFLAGS = -gnat2022 -O2
LDFLAGS =

build/day_01: src/day_01.adb
	@mkdir -p $(dir $@)
	gnatmake -D build $^ -o $@ $(ADAFLAGS) $(LDFLAGS)
