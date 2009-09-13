EBIN_DIR=../ebin
INCLUDE_DIR=../include
ERLS=$(wildcard *.erl)
ERLC=erlc -o $(EBIN_DIR) -I ${INCLUDE_DIR}
BEAMS=$(ERLS:.erl=.beam)

# leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
	$(ERLC) -W $<

# Here's a list of the erlang modules you want compiling
# If the modules don't fit onto one line add a \ character 
# to the end of the line and continue on the next line

# Edit the lines below
#MODS = army battle city counter db entity  \
#       game_loop game login map object \
#       packet pickle player server test \
#	   unit util

BEAMS = $(ERLS:.erl=.beam)
MODS = $(BEAMS:.beam=)

all:	compile

%.beam: %.erl
	@echo ">>" compiling: $<
	@$(ERLC) $<

compile: ${MODS:%=%.beam}

clean:
	rm -rf ../ebin/*.beam erl_crash.dump
