OBJ = chicken-coverage.o
BIN = $(OBJ:.o=)

# Debug build.
CSCFLAGS = -O0 -d2 -v -local

# Normal build.
#CSCFLAGS = -O3 -d0 -local

.SUFFIXES: .scm

all: chicken-coverage

chicken-coverage: chicken-coverage.scm

.scm:
	csc $(CSCFLAGS) -o $@ $<

clean:
	rm -f $(BIN) $(OBJ)

install: $(BIN)
	install -d $(DESTDIR)$(PREFIX)/bin
	install -m 755 $(BIN) $(DESTDIR)$(PREFIX)/bin
