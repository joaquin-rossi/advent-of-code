.PHONY: all build clean

all: build

build: build/day_01 build/day_02 build/day_03 build/day_04

clean:
	rm -rf build

ADAFLAGS = -gnat2022 -O2
LDFLAGS =

.PHONY: src/day_01.adb src/day_02.adb src/day_03.adb src/day_04.adb

build/%: src/%.adb
	@mkdir -p $(dir $@)
	gnatmake -D build $^ -o $@ $(ADAFLAGS) $(LDFLAGS)
