use criterion::{black_box, criterion_group, criterion_main, Criterion};
use nextver::prelude::*;

fn sem_ok_inputs() -> Vec<&'static str> {
    vec![
        "[MAJOR].[MINOR].[PATCH]",
        "[MAJOR].[MINOR]",
        "[MAJOR]",
    ]
}

fn parse_sem_ok(inputs: &[&str]) {
    for input in inputs {
        let res = Sem::new_format(input);
        assert!(res.is_ok());
    }
}

fn cal_ok_inputs() -> Vec<&'static str> {
    vec![
        "[YYYY].[MM].[DD]",
        "[YYYY].[MM]",
        "[YYYY]",
    ]
}

fn parse_cal_ok(inputs: &[&str]) {
    for input in inputs {
        let res = Cal::new_format(input);
        assert!(res.is_ok());
    }
}

fn cal_sem_ok_inputs() -> Vec<&'static str> {
    vec![
        "[YYYY].[MM].[MINOR].[PATCH]",
        "[YYYY].[MM].[MINOR]",
        "[YYYY].[MM].[DD].[MINOR].[PATCH]",
        "[YYYY].[MM].[DD].[MINOR]",
        "[YYYY].[WW].[MINOR].[PATCH]",
        "[YYYY].[WW].[MINOR]",
    ]
}

fn parse_cal_sem_ok(inputs: &[&str]) {
    for input in inputs {
        let res = CalSem::new_format(input);
        assert!(res.is_ok());
    }
}

fn new_cal_sem_version_inputs() -> Vec<(&'static str, &'static str)> {
    vec![
        ("[YYYY].[MM].[MINOR].[PATCH]", "2020.01.01.01"),
        ("[YYYY].[MM].[MINOR]", "2020.01.01"),
        ("[YYYY].[MM].[DD].[MINOR]", "2020.01.01"),
        ("[YYYY].[WW].[MINOR].[PATCH]", "2020.01.01"),
        ("[YYYY].[WW].[MINOR]", "2020.01.01"),
    ]
}

fn new_cal_sem_version(inputs: &[(&str, &str)]) {
    for (format_str, version_str) in inputs {
        let res = CalSem::new_version(format_str, version_str);
        assert!(res.is_ok());
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("parse_sem_ok", |b| b.iter(|| parse_sem_ok(black_box(&sem_ok_inputs()))));
    c.bench_function("parse_cal_ok", |b| b.iter(|| parse_cal_ok(black_box(&cal_ok_inputs()))));
    c.bench_function("parse_cal_sem_ok", |b| b.iter(|| parse_cal_sem_ok(black_box(&cal_sem_ok_inputs()))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
