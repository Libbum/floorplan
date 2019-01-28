TARGETS := dist/floorplan.js dist/floorplan.css

.PHONY: clean build rebuild prod

rebuild: clean build

clean:
	@-rm -f $(TARGETS)

build: src/Main.elm
	elm make src/Main.elm --output=dist/floorplan.js --optimize

serve: src/Main.elm
	elm-live src/Main.elm -d dist --pushstate --open -- --output=dist/floorplan.js --optimize

debug: src/Main.elm
	elm-live src/Main.elm -d dist --pushstate --open -- --output=dist/floorplan.js --debug

dist/floorplan.min.js: build
	uglifyjs dist/floorplan.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=dist/floorplan.min.js

prodjs: dist/floorplan.min.js
	mv dist/floorplan.min.js dist/floorplan.js

prodcs: src/floorplan.css
	crass src/floorplan.css --optimize > dist/floorplan.css

prod: prodjs prodcs

deploy: prod
	rsync -avr --chown=www-data:www-data --checksum --delete -e ssh dist/ AkashaR:neophilus/floorplan
