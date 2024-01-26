use std::time::Duration;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use nextver::prelude::*;

fn format_sem_inputs() -> Vec<&'static str> {
    vec!["[MAJOR].[MINOR].[PATCH]", "[MAJOR].[MINOR]", "[MAJOR]"]
}

fn format_sem(inputs: &[&str]) {
    for input in inputs {
        let res = Sem::new_format(input);
        assert!(res.is_ok());
    }
}

fn format_cal_inputs() -> Vec<&'static str> {
    vec!["[YYYY].[MM].[DD]", "[YYYY].[MM]", "[YYYY]"]
}

fn format_cal(inputs: &[&str]) {
    for input in inputs {
        let res = Cal::new_format(input);
        assert!(res.is_ok());
    }
}

fn format_calsem_inputs() -> Vec<&'static str> {
    vec![
        "[YYYY].[MM].[MINOR].[PATCH]",
        "[YYYY].[MM].[MINOR]",
        "[YYYY].[MM].[DD].[MINOR].[PATCH]",
        "[YYYY].[MM].[DD].[MINOR]",
        "[YYYY].[WW].[MINOR].[PATCH]",
        "[YYYY].[WW].[MINOR]",
    ]
}

fn format_calsem(inputs: &[&str]) {
    for input in inputs {
        let res = CalSem::new_format(input);
        assert!(res.is_ok());
    }
}

fn version_calsem_inputs() -> Vec<(&'static str, &'static str)> {
    vec![
        ("[YYYY].[MM].[MINOR].[PATCH]", "2020.1.1.1263"),
        ("[YY].[MM].[MINOR]", "20.1.633"),
        ("[0Y].[0M].[DD].[MINOR]", "09.01.23.352"),
        ("[YYYY].[0W].[MINOR].[PATCH]", "2020.01.2341.35"),
        ("[YY].[WW].[MINOR]", "20.5.22345"),
    ]
}

fn version_calsem(inputs: &[(&str, &str)]) {
    for (format_str, version_str) in inputs {
        let res = CalSem::new_version(format_str, version_str);
        assert!(res.is_ok());
    }
}

fn format_benchmark(c: &mut Criterion) {
    c.bench_function("format_sem", |b| {
        b.iter(|| format_sem(black_box(&format_sem_inputs())))
    });
    c.bench_function("format_cal", |b| {
        b.iter(|| format_cal(black_box(&format_cal_inputs())))
    });
    c.bench_function("format_calsem", |b| {
        b.iter(|| format_calsem(black_box(&format_calsem_inputs())))
    });
}

fn version_benchmark(c: &mut Criterion) {
    c.bench_function("version_calsem", |b| {
        b.iter(|| version_calsem(black_box(&version_calsem_inputs())))
    });
}

criterion_group!{
    name = benches;
    // give us more warmup time. my computer seems to do better with more warmup time
    config = Criterion::default().warm_up_time(Duration::from_secs(7));
    targets = format_benchmark, version_benchmark
}
criterion_main!(benches);

// TODO: look at how chrono does their criterion benchmarking. our structure could be improved.