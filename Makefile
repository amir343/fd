ERLC=erlc -pa ebin -o ebin -I include +debug_info

.PHONY: all
all: compile

.PHONY: compile
compile: clean
	$(ERLC) src/*.erl

.PHONY: clean
clean:
	rm -fr ebin/*.beam
