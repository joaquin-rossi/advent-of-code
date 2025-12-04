.PHONY: all build clean

all: build

build: build/day_01 build/day_02

clean:
	rm -rf build

ADAFLAGS = -gnat2022 -O2
LDFLAGS =

build/%: src/%.adb
	@mkdir -p $(dir $@)
	gnatmake -D build $^ -o $@ $(ADAFLAGS) $(LDFLAGS)
