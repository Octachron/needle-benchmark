#!/usr/bin/env sh


parameters="-n-iteration 10_000 -data random -shared true -needle-size 100 "
hyperfine --warmup 10 \
     "./bench.exe -search naive $parameters"\
     "./bench.exe -search kmp $parameters"\
     "./bench.exe -search two-way $parameters"
