use criterion::{black_box, criterion_group, criterion_main, Criterion};
use nextver::prelude::*;

fn bench_format_sem(c: &mut Criterion) {
    let format_str = "[MAJOR].[MINOR].[PATCH]";
    c.bench_function("bench_format_sem", |b| {
        b.iter(|| Sem::new_format(black_box(format_str)).unwrap())
    });
}

fn bench_format_cal(c: &mut Criterion) {
    let format_str = "[YYYY].[MM].[DD]";
    c.bench_function("bench_format_cal", |b| {
        b.iter(|| Cal::new_format(black_box(format_str)).unwrap())
    });
}

fn bench_format_calsem(c: &mut Criterion) {
    let format_str = "[YYYY].[MM].[DD]-[MINOR].[PATCH]";
    c.bench_function("bench_format_calsem", |b| {
        b.iter(|| CalSem::new_format(black_box(format_str)).unwrap())
    });
}

fn bench_format_sem_large_literal(c: &mut Criterion) {
    let format_str = "Tell me, Muse, of the man of many ways, who was driven[MAJOR]far journeys, after he had sacked Troy's sacred citadel.[MINOR]\"The Odyssey\" by Homer[PATCH]👯‍♀️";
    c.bench_function("bench_format_sem_large_literal", |b| {
        b.iter(|| Sem::new_format(black_box(format_str)).unwrap())
    });
}

fn bench_version_sem(c: &mut Criterion) {
    let format_str = "[MAJOR].[MINOR].[PATCH]";
    let version_str = "5.0.12390";
    c.bench_function("bench_version_sem", |b| {
        b.iter(|| Sem::new_version(black_box(format_str), version_str).unwrap())
    });
}

fn bench_version_cal(c: &mut Criterion) {
    let format_str = "[YYYY].[WW]";
    let version_str = "2020.47";
    c.bench_function("bench_version_cal", |b| {
        b.iter(|| Cal::new_version(black_box(format_str), version_str).unwrap())
    });
}

fn bench_version_calsem(c: &mut Criterion) {
    let format_str = "[YYYY].[MM].[DD]-[MINOR].[PATCH]";
    let version_str = "2020.10.27-1.1263";
    c.bench_function("bench_version_calsem", |b| {
        b.iter(|| CalSem::new_version(black_box(format_str), version_str).unwrap())
    });
}

fn bench_next_sem(c: &mut Criterion) {
    let format_str = "[MAJOR].[MINOR].[PATCH]";
    let version_str = "5.0.12390";
    let spec = SemSpecifier::Minor;
    c.bench_function("bench_next_sem", |b| {
        b.iter(|| Sem::next_string(black_box(format_str), version_str, &spec).unwrap())
    });
}

fn bench_next_cal(c: &mut Criterion) {
    let format_str = "[YYYY].[WW]";
    let version_str = "2020.47";
    let date = Date::explicit(2024, 1, 27).unwrap();
    c.bench_function("bench_next_cal", |b| {
        b.iter(|| Cal::next_string(black_box(format_str), version_str, &date).unwrap())
    });
}

fn bench_next_calsem(c: &mut Criterion) {
    let format_str = "[YYYY].[MM].[DD]-[MINOR].[PATCH]";
    let version_str = "2020.10.27-0.1263";
    let date = Date::explicit(2024, 1, 27).unwrap();
    let spec = CalSemIncrSpecifier::Minor;
    c.bench_function("bench_next_calsem", |b| {
        b.iter(|| CalSem::next_string(black_box(format_str), version_str, &date, &spec).unwrap())
    });
}

criterion_group!(
    benches,
    bench_format_sem,
    bench_format_cal,
    bench_format_calsem,
    bench_format_sem_large_literal,
    bench_version_sem,
    bench_version_cal,
    bench_version_calsem,
    bench_next_sem,
    bench_next_cal,
    bench_next_calsem
);
criterion_main!(benches);
