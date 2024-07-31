// Code is from shrektan/ymd
use chrono::{Datelike, NaiveDate};

fn from_ymd(year: i32, month: u32, day: u32) -> NaiveDate {
    return NaiveDate::from_ymd_opt(year, month, day).expect("invalid or out-of-range date");
}

pub fn add_months(ref_date: &NaiveDate, months: i32) -> NaiveDate {
    let num_of_months = ref_date.year() * 12 + ref_date.month() as i32 + months as i32;
    let year = (num_of_months - 1) / 12;
    let month = (num_of_months - 1) % 12 + 1;
    let since = NaiveDate::signed_duration_since;
    let nxt_month = if month == 12 {
        from_ymd(year + 1, 1 as u32, 1)
    } else {
        from_ymd(year, (month + 1) as u32, 1)
    };
    let max_day = since(nxt_month, from_ymd(year, month as u32, 1)).num_days() as u32;
    let day = ref_date.day();
    from_ymd(
        year,
        month as u32,
        if day > max_day { max_day } else { day },
    )
}

// this is similiar to 30 / 360 rule
pub fn year_frac(d1: &NaiveDate, d0: &NaiveDate) -> f64 {
    (d1.year() - d0.year()) as f64
    // must be as f64 first, otherwise u32 - u32 may overflow (when negative)
        + (d1.month() as f64 - d0.month() as f64) / 12.0
        + (d1.day() as f64 - d0.day() as f64) / 365.0
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn add_months_work() {
        let ref_date = from_ymd(2020, 12, 31);
        assert_eq!(add_months(&ref_date, 0), ref_date);
        assert_eq!(add_months(&ref_date, 1), from_ymd(2021, 1, 31));
        assert_eq!(add_months(&ref_date, 2), from_ymd(2021, 2, 28));
        assert_eq!(add_months(&ref_date, 11), from_ymd(2021, 11, 30));
        assert_eq!(add_months(&ref_date, 12), from_ymd(2021, 12, 31));
    }
}
