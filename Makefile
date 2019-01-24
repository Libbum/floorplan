TARGETS := dist/floorplan.js

.PHONY: clean build rebuild

rebuild: clean build

clean:
	@-rm -f $(TARGETS)

build: src/Main.elm
	elm make src/Main.elm --output=dist/floorplan.js --optimize

serve: src/Main.elm
	elm-live src/Main.elm -d dist --pushstate --open -- --output=dist/floorplan.js --optimize

debug: src/Main.elm
	elm-live src/Main.elm -d dist --pushstate --open -- --output=dist/floorplan.js --debug

