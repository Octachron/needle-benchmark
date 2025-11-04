#!/usr/bin/env sh



parameters="-mode replace-all -n-iteration 10_00 -data random -shared false -needle-size 100 "
hyperfine --warmup 10 \
     "./bench.exe -search naive $parameters"\
     "./bench.exe -search two-way $parameters"
