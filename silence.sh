#!/bin/sh
exec erl -detached -sname mmo -pz ./ebin ./lib/log4erl/ebin ./lib/map_port/ebin -s server start
