#!/bin/sh

erl -sname theNetwork -pa ebin -pa deps/*/ebin -s theNetwork_app
