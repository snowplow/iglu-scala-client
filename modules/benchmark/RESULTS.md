# Native vs Networknt Validator Benchmark Results

JDK 21.0.2, OpenJDK 64-Bit Server VM, macOS, single thread.
Dataset: Draft-04 meta-schema (4.1 KB) + 563 JSON instances (7 MB).
Identical benchmark code on both branches using `CirceValidator` API.

## Throughput (ops/s, higher is better)

| Benchmark | Networknt (develop) | Native (feature) | Speedup |
|-----------|--------------------:|-----------------:|--------:|
| validate (pre-compiled, 563 instances) | 22.0 | 165.9 | 7.5x |
| compileAndValidate (compile+validate, 563 instances) | 10.7 | 76.1 | 7.1x |
| checkSchema | 30,217 | 238,233 | 7.9x |

## Memory Allocation (B/op, lower is better)

| Benchmark | Networknt (develop) | Native (feature) | Reduction |
|-----------|--------------------:|-----------------:|----------:|
| validate (pre-compiled, 563 instances) | 185,178,915 | 13,398,122 | 13.8x |
| compileAndValidate (compile+validate, 563 instances) | 463,871,528 | 48,348,394 | 9.6x |
| checkSchema | 129,616 | 15,400 | 8.4x |

## Raw JMH Output

### Native (feature/native-json-schema-validator)

```
Benchmark                                                Mode  Cnt         Score      Error   Units
ValidatorBenchmark.checkSchema                          thrpt   10    238233.066 ± 7335.834   ops/s
ValidatorBenchmark.checkSchema:gc.alloc.rate.norm       thrpt   10     15400.029 ±    0.001    B/op
ValidatorBenchmark.compileAndValidate                   thrpt   10        76.135 ±    0.461   ops/s
ValidatorBenchmark.compileAndValidate:gc.alloc.rate.norm thrpt  10  48348394.300 ±    0.567    B/op
ValidatorBenchmark.validate                             thrpt   10       165.912 ±    1.748   ops/s
ValidatorBenchmark.validate:gc.alloc.rate.norm          thrpt   10  13398121.632 ±    0.427    B/op
```

### Networknt (develop)

```
Benchmark                                                Mode  Cnt          Score      Error   Units
ValidatorBenchmark.checkSchema                          thrpt   10      30216.547 ±   82.537   ops/s
ValidatorBenchmark.checkSchema:gc.alloc.rate.norm       thrpt   10     129616.229 ±    0.001    B/op
ValidatorBenchmark.compileAndValidate                   thrpt   10         10.671 ±    0.247   ops/s
ValidatorBenchmark.compileAndValidate:gc.alloc.rate.norm thrpt  10  463871528.000 ±   81.529    B/op
ValidatorBenchmark.validate                             thrpt   10         22.001 ±    1.070   ops/s
ValidatorBenchmark.validate:gc.alloc.rate.norm          thrpt   10  185178915.378 ±   14.325    B/op
```

## How to Reproduce

```bash
sbt "benchmark/Jmh/run -i 10 -wi 5 -f 1 -t 1 -prof gc .*client.*ValidatorBenchmark"
```
