SOURCE := $(wildcard src/*.scala)

all: .compiled

.compiled: $(SOURCE)
	@mkdir -p bin
	$(SCALAC) -d bin $^
	touch $@


clean: force
	rm -rf .compiled bin 

force:

SCALAC = scalac -cp "../lib/*"
