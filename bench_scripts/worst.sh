#!/usr/bin/env sh


 hyperfine --warmup 10 \
     "./bench.exe -search naive -n-iteration 10_000 -data worst"\
     "./bench.exe -search kmp -n-iteration 10_000 -data worst"\
     "./bench.exe -search two-way -n-iteration 10_000 -data worst"
