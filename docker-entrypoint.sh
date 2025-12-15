#!/bin/bash
# Copyright 2025 Carnegie Mellon University

exec sbcl --dynamic-space-size 20000 --load quicklisp/setup.lisp --load act-up-v1_3_3 --load IBL --load http-server.lisp --eval '(jh:run-standalone)'
