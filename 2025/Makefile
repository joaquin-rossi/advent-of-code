.PHONY: all build clean

all: build

build: \
	build/day_01 \
	build/day_02 \
	build/day_03 \
	build/day_04 \
	build/day_04_viz \
	build/day_05 \
	build/day_06

clean:
	rm -rf build

ADAFLAGS = -gnat2022 -O2 -gnatf
LDFLAGS =

.PHONY: $(shell find src -name '*.adb')

build/%: src/%.adb
	@mkdir -p $(dir $@)
	gnatmake -D build $^ -o $@ $(ADAFLAGS) $(LDFLAGS)
