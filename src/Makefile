EBIN_DIR=../ebin
INCLUDE_DIR=../include
ERLS=$(wildcard *.erl)
ERLC=erlc -I ${INCLUDE_DIR}
BEAMS=$(ERLS:.erl=.beam)

# leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
	$(ERLC) -W $<

# Here's a list of the erlang modules you want compiling
# If the modules don't fit onto one line add a \ character 
# to the end of the line and continue on the next line

# Edit the lines below
MODS = army battle city counter db entity event\
       game_loop game improvement login map object \
       packet pickle player population server subscription\
       t transport test unit util

#BEAMS = $(ERLS:.erl=.beam)
#MODS = $(BEAMS:.beam=)

all:	compile

compile: ${MODS:%=%.beam}
	cp *.beam ../ebin
#%.beam: %.erl
#	@echo ">>" compiling: $<
#	@$(ERLC) $<

clean:
	rm -rf ../ebin/*.beam erl_crash.dump
