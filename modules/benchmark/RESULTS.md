# Native vs Networknt Validator Benchmark Results

JDK 21.0.2, OpenJDK 64-Bit Server VM, macOS, single thread.
Dataset: Draft-04 meta-schema (4.1 KB) + 563 JSON instances (7 MB).
Per-instance compile+validate on both branches; `-i 5 -wi 3 -f 3 -t 1`.

## Throughput (ops/s, higher is better)

| Benchmark | Networknt (develop) | Native (feature) | Speedup |
|-----------|--------------------:|-----------------:|--------:|
| validate (pre-compiled, 563 instances) | 21.0 | 180.9 | 8.6x |
| compileAndValidate (compile per instance, 563 instances) | 10.3 | 78.2 | 7.6x |
| checkSchema | 27,846 | 245,467 | 8.8x |

## Memory Allocation (B/op, lower is better)

| Benchmark | Networknt (develop) | Native (feature) | Reduction |
|-----------|--------------------:|-----------------:|----------:|
| validate (pre-compiled, 563 instances) | 185,178,915 | 6,887,999 | 26.9x |
| compileAndValidate (compile per instance, 563 instances) | 463,871,528 | 44,781,651 | 10.4x |
| checkSchema | 129,616 | 12,115 | 10.7x |

## Raw JMH Output

### Native (feature/native-json-schema-validator)

```
Benchmark                                                  Mode  Cnt         Score         Error   Units
ValidatorBenchmark.checkSchema                            thrpt   15    249545.840 ±    6369.317   ops/s
ValidatorBenchmark.checkSchema:gc.alloc.rate.norm         thrpt   15     12114.694 ±    1068.336    B/op
ValidatorBenchmark.compileAndValidate                     thrpt   15        79.854 ±       1.834   ops/s
ValidatorBenchmark.compileAndValidate:gc.alloc.rate.norm  thrpt   15  44781651.441 ± 1730580.882    B/op
ValidatorBenchmark.validate                               thrpt   15       190.145 ±       7.459   ops/s
ValidatorBenchmark.validate:gc.alloc.rate.norm            thrpt   15   6887998.990 ±  920104.957    B/op
```

### Networknt (develop)

```
Benchmark                                                Mode  Cnt          Score      Error   Units
ValidatorBenchmark.checkSchema                          thrpt   15      27846.288 ±  142.986   ops/s
ValidatorBenchmark.checkSchema:gc.alloc.rate.norm       thrpt   15     129616.229 ±    0.001    B/op
ValidatorBenchmark.compileAndValidate                   thrpt   15         10.337 ±    0.251   ops/s
ValidatorBenchmark.compileAndValidate:gc.alloc.rate.norm thrpt  15  463871528.000 ±   81.529    B/op
ValidatorBenchmark.validate                             thrpt   15         21.013 ±    0.713   ops/s
ValidatorBenchmark.validate:gc.alloc.rate.norm          thrpt   15  185178915.378 ±   14.325    B/op
```

## How to Reproduce

```bash
sbt "project benchmark" "Jmh/run -i 5 -wi 3 -f 3 -t 1 ValidatorBenchmark"
```
