#!/bin/sh
cd $(dirname $0)
rm -rf log/*
exec erl \
    +P 5000000 \
    +K true \
    -pa ebin \
    -s env \
    -sname env
