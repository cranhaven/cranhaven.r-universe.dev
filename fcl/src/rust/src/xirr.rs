// The code is copied from https://github.com/raymon1/financial with some modifications
// financial's xirr at current doesn't provide NaiveDate api and use days/365.0 to calculate
// the year fractions, which is slightly different from what expected normally
// https://github.com/raymon1/financial/issues/9
use crate::date_handle;
use crate::find_root::*;
use chrono::NaiveDate;

pub struct CheckedCashflowSchedule<'a> {
    pub values: &'a [f64],
    pub dates: &'a [NaiveDate],
}

impl<'a> CheckedCashflowSchedule<'a> {
    pub fn new(
        values: &'a [f64],
        dates: &'a [NaiveDate],
    ) -> Result<CheckedCashflowSchedule<'a>, &'static str> {
        if values.len() != dates.len() {
            return Err("Values and dates length must match");
        }
        let d0 = dates.first().unwrap();
        if dates.iter().any(|d| *d < *d0) {
            return Err("First date must be the earliest");
        };
        Ok(CheckedCashflowSchedule { values, dates })
    }
}

pub fn xnpv(rate: f64, values: &[f64], dates: &[NaiveDate]) -> Result<f64, &'static str> {
    let cf = CheckedCashflowSchedule::new(values, dates);
    match cf {
        Err(m) => Err(m),
        Ok(cf) => Ok(calculate_xnpv(rate, &cf)),
    }
}

pub fn calculate_xnpv(rate: f64, cf: &CheckedCashflowSchedule) -> f64 {
    if cf.values.is_empty() {
        return 0.;
    }

    if rate == 0. {
        return cf.values.iter().sum();
    }

    let d0 = cf.dates.first().unwrap();
    cf.values
        .iter()
        .zip(cf.dates.iter())
        .map(|(v, d)| v / f64::powf(1. + rate, date_handle::year_frac(d, &d0)))
        .sum()
}

pub fn xirr(values: &[f64], dates: &[NaiveDate], guess: Option<f64>) -> Result<f64, &'static str> {
    let cf = CheckedCashflowSchedule::new(values, dates);

    match cf {
        Err(m) => Err(m),
        Ok(cf) => {
            let f_xnpv = |x: f64| calculate_xnpv(x, &cf);
            match find_root(guess, f_xnpv, 1.1) {
                Some(ans) => Ok(ans),
                None => Err("could't find irr for the values provided"),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert::NearEq;
    use chrono::{Duration, NaiveDate};
    fn from_ymd(year: i32, month: u32, day: u32) -> NaiveDate {
        return NaiveDate::from_ymd_opt(year, month, day).expect("invalid or out-of-range date");
    }
    #[test]
    fn xnpv_with_zero_rate() {
        let cf: [f64; 10000] = [100.; 10000];
        let somedate: NaiveDate = from_ymd(2021, 1, 1);
        let dates0: [NaiveDate; 10000] = [somedate; 10000];
        let mut dates: [NaiveDate; 10000] = [somedate; 10000];

        let mut i = 0;
        for d in dates0.iter() {
            dates[i] = d
                .checked_add_signed(Duration::weeks(52 * (i as i64)))
                .unwrap();
            i = i + 1;
        }

        assert_eq!(xnpv(0., &cf, &dates).unwrap(), cf.iter().sum::<f64>());
    }

    #[test]
    fn xirr_test() {
        // non leap year
        let cf = [-100., 105.];
        let dates = [from_ymd(2021, 1, 1), from_ymd(2022, 1, 1)];
        assert_near_eq!(xirr(&cf, &dates, None).unwrap(), 0.05);
        // leap year
        let cf = [-100., 105.];
        let dates = [from_ymd(2020, 1, 1), from_ymd(2021, 1, 1)];
        assert_near_eq!(xirr(&cf, &dates, None).unwrap(), 0.05);
    }
}
