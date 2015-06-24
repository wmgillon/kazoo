#!/bin/bash

cd ../../ && \
    ./scripts/sup-build-autocomplete.escript applications/ core/ && \
    mv sup_mfas.erl utils/sup/src/ && \
    cd -

../rebar/rebar clean compile escriptize
