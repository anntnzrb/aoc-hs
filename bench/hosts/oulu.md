# Measurement host

Host: oulu
OS/kernel/architecture: Linux 7.2.8 x86_64
CPU: 13th Gen Intel(R) Core(TM) i7-1355U
Core types: P=0-3; E=4-11; pinned CPU=0
perf_event_paranoid: 2
Compiler: ghc 9.10.3 (GHC2024, cabal -O1)
cabal: 3.16.1.0
taskset: taskset from util-linux 2.42.3
perf: perf version 7.2.9
GNU time: time (GNU Time) 1.10

Wall: separate runs of the solver under taskset only (no wrapper on macOS); monotonic elapsed microseconds include process spawn and stdin/stdout piping.
CPU: separate taskset perf stat -- solver runs (no GNU time), interleaved with wall runs and across variants; default user+sys seconds include solver startup, printed to 1 ns, stored at 1 us resolution. If any sample is missing or below 1 us, the entire CPU statistic is unavailable; GNU time's 10 ms CPU fields are not used.
RSS: separate taskset GNU time -f %M -- solver runs (no taskset on macOS); child peak resident set in KiB, maximum across runs; includes solver runtime startup, no CPU fields used.
Work: taskset perf stat -x, -e instructions:u -- solver runs, full minus empty, exactly five interleaved pairs; hardware counts may vary; no generated-code mechanism inferred.
