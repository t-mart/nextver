use criterion::{black_box, criterion_group, criterion_main, Criterion};
use version_bump::Format;

fn ok_inputs() -> Vec<&'static str> {
    vec![
        "[MAJOR].[MINOR].[PATCH]",
        "[YYYY].[MM].[DD]",
        "[MAJOR].[MINOR].[PATCH]-[YYYY].[YY].[0Y].[MM].[0M].[WW].[0W].[DD].[0D]",
    ]
}

fn parse_ok(inputs: &[&str]) {
    for input in inputs {
        let res = Format::parse(input);
        assert!(res.is_ok());
    }
}

fn unknown_spec_inputs() -> Vec<&'static str> {
    vec![
        "[MAJOR].[MINOR].[PATCH].[FOO]",
        "[YYYY].[MM].[DD].[FOO]",
        "[MAJOR].[MINOR].[PATCH]-[YYYY].[YY].[0Y].[MM].[0M].[WW].[0W].[DD].[0D].[FOO]",
    ]
}

fn parse_unknown_spec(inputs: &[&str]) {
    for input in inputs {
        let res = Format::parse(input);
        assert!(res.is_err());
    }
}

fn unterminated_spec_inputs() -> Vec<&'static str> {
    vec![
        "[MAJOR].[MINOR].[PATCH",
        "[YYYY].[MM].[DD",
        "[MAJOR].[MINOR].[PATCH]-[YYYY].[YY].[0Y].[MM].[0M].[WW].[0W].[DD].[0D",
    ]
}

fn parse_unterminated_spec(inputs: &[&str]) {
    for input in inputs {
        let res = Format::parse(input);
        assert!(res.is_err());
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("parse_ok", |b| b.iter(|| parse_ok(black_box(&ok_inputs()))));
    c.bench_function("parse_unknown", |b| {
        b.iter(|| parse_unknown_spec(black_box(&unknown_spec_inputs())))
    });
    c.bench_function("parse_unterminated", |b| {
        b.iter(|| parse_unterminated_spec(black_box(&unterminated_spec_inputs())))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
