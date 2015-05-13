all: compile app

compile:
	(cd src; make)

app:
	./buildapp.sh

clean:
	(cd src; make clean)

.PHONY: all compile app clean

.SILENT: all compile app clean


