use std::collections::BTreeMap;

type RDate = i32;

pub struct Rtn {
    dates: Vec<RDate>,
    mvs: Vec<f64>,
    pls: Vec<f64>,
}

impl Rtn {
    pub fn new(dates: Vec<RDate>, mvs: Vec<f64>, pls: Vec<f64>) -> Result<Self, String> {
        check_len!(dates, mvs, pls);
        let mut data: BTreeMap<RDate, (f64, f64)> = BTreeMap::new();
        for (i, date) in dates.iter().enumerate() {
            if data.contains_key(&date) {
                return Err("dates contain duplicate".to_string());
            }
            data.insert(*date, (mvs[i], pls[i]));
        }
        let min_date = *data.keys().min().unwrap();
        let max_date = *data.keys().max().unwrap();
        let dates: Vec<RDate> = (min_date..=max_date).collect();
        let keys: Vec<RDate> = data.keys().cloned().collect();
        let mut mvs: Vec<f64> = Vec::new();
        let mut pls: Vec<f64> = Vec::new();
        for d in dates.iter() {
            match keys.binary_search(d) {
                Ok(i) => {
                    let v = data.get(&keys[i]).unwrap();
                    mvs.push(v.0);
                    pls.push(v.1);
                }
                Err(i) => {
                    let v = data.get(&keys[i - 1]).unwrap();
                    mvs.push(v.0);
                    pls.push(0.0);
                }
            };
        }
        Ok(Self { dates, mvs, pls })
    }
    fn mv(&self, i: usize) -> Option<&f64> {
        self.mvs.get(i)
    }
    fn mv0(&self, i: usize) -> Option<&f64> {
        if i == 0 {
            None
        } else {
            self.mv(i - 1)
        }
    }
    fn pl(&self, i: usize) -> Option<&f64> {
        self.pls.get(i)
    }
    fn cf(&self, i: usize) -> Option<f64> {
        Some(self.mv(i)? - self.mv0(i)? - self.pl(i)?)
    }
    fn dr(&self, i: usize) -> Option<f64> {
        let cf = self.cf(i)?;
        let cf_use = if cf >= 0.0 { cf } else { 0.0 };
        let deno = self.mv0(i)? + cf_use;
        match deno.classify() {
            std::num::FpCategory::Normal => Some(self.pl(i)? / deno),
            _ => None,
        }
    }
    fn crs(drs: &Vec<Option<f64>>) -> Vec<Option<f64>> {
        let mut out: Vec<Option<f64>> = Vec::with_capacity(drs.len());
        for (i, dr) in drs.iter().enumerate() {
            let v = if i == 0 {
                *dr
            } else {
                if dr.is_none() || out[i - 1].is_none() {
                    None
                } else {
                    Some((out[i - 1].unwrap() + 1.) * (dr.unwrap() + 1.) - 1.)
                }
            };
            out.push(v);
        }
        out
    }
    fn i(&self, date: RDate) -> Option<usize> {
        match self.dates.binary_search(&date) {
            Ok(k) => Some(k),
            Err(_) => None,
        }
    }
    pub fn dates(from: RDate, to: RDate) -> Result<Vec<RDate>, String> {
        if from > to {
            return Err("from should be equal or smaller than to".to_string());
        }
        Ok((from..=to).collect())
    }
    fn i_dates(&self, from: RDate, to: RDate) -> Result<Vec<usize>, String> {
        let i_from = self.i(from).ok_or("from is out range")?;
        let i_to = self.i(to).ok_or("to is out range")?;
        if i_from > i_to {
            return Err("from should be equal or smaller than to".to_string());
        }
        Ok((i_from..=i_to).collect())
    }
    pub fn twrr_dr(&self, from: RDate, to: RDate) -> Result<Vec<Option<f64>>, String> {
        let i_dates = self.i_dates(from, to)?;
        let drs = i_dates.iter().map(|i| self.dr(*i)).collect();
        Ok(drs)
    }
    pub fn twrr_cr(&self, from: RDate, to: RDate) -> Result<Vec<Option<f64>>, String> {
        let mut out = self.twrr_dr(from, to)?;
        out = Self::crs(&out);
        Ok(out)
    }
    fn weighted_cf(i_dates: &[usize], cfs: &[Option<f64>], i: usize) -> Option<f64> {
        if i_dates.len() != cfs.len() {
            panic!("the len of i_dates and cfs doesn't equal");
        }
        let i_dates = i_dates.get(0..=i).unwrap();
        let cfs = cfs.get(0..=i).unwrap();
        let any_na: bool = cfs.iter().any(|x| x.is_none());
        if any_na {
            return None;
        }
        let total_days = i_dates.last().unwrap() - i_dates.first().unwrap() + 1;
        let weighted_cf: f64 = cfs
            .iter()
            .zip(i_dates)
            .map(|(cf, i)| {
                let cf = cf.unwrap();
                // this is the same handling as twrr dr. when cf is positive, it assumes it happens at
                // the BOP of the day. when it's negative, EOP. This makes the calculation more robust,
                // as it reduces the chance that the denominator is close to zero.
                let wt: f64 =
                    (i_dates.last().unwrap() - i + (cf > 0.0) as usize) as f64 / total_days as f64;
                cf * wt
            })
            .sum();
        Some(weighted_cf)
    }
    pub fn dietz_avc(&self, from: RDate, to: RDate) -> Result<Vec<Option<f64>>, String> {
        let i_dates = self.i_dates(from, to)?;
        let mv0: f64 = *self.mv0(i_dates[0]).ok_or("can't fetch mv0")?;
        let cfs: Vec<Option<f64>> = i_dates.iter().map(|i| self.cf(*i)).collect();
        let out: Vec<Option<f64>> = i_dates
            .iter()
            .enumerate()
            .map(|(i, _)| match Self::weighted_cf(&i_dates, &cfs, i) {
                Some(v) => Some(v + mv0),
                None => None,
            })
            .collect();
        Ok(out)
    }
    pub fn cum_pl(&self, from: RDate, to: RDate) -> Result<Vec<Option<f64>>, String> {
        let i_dates = self.i_dates(from, to)?;
        let pls: Vec<Option<f64>> = i_dates
            .iter()
            .map(|i| match self.pl(*i) {
                Some(v) => Some(*v),
                None => None,
            })
            .collect();
        let mut cum_pls: Vec<Option<f64>> = Vec::with_capacity(pls.len());
        for (i, pl) in pls.iter().enumerate() {
            if i == 0 {
                cum_pls.push(*pl);
            } else {
                match (pl, cum_pls[i - 1]) {
                    (Some(v), Some(v0)) => {
                        cum_pls.push(Some(v0 + v));
                    }
                    _ => {
                        cum_pls.push(None);
                    }
                }
            }
        }
        Ok(cum_pls)
    }
    pub fn dietz(&self, from: RDate, to: RDate) -> Result<Vec<Option<f64>>, String> {
        let cum_pls = self.cum_pl(from, to)?;
        let avcs: Vec<Option<f64>> = self.dietz_avc(from, to)?;
        let out: Vec<Option<f64>> = cum_pls
            .iter()
            .zip(avcs)
            .map(|(pl, avc)| match (pl, avc) {
                (Some(pl), Some(avc)) => match avc.classify() {
                    std::num::FpCategory::Normal => Some(pl / avc),
                    _ => None,
                },
                _ => None,
            })
            .collect();
        Ok(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert::NearEq;
    #[test]
    fn twrr_work() {
        let dates = vec![1, 3, 4, 5];
        let mvs = vec![100., 102., 103., 104.];
        let pls = vec![0., 2., 1., 1.];
        let rtn = Rtn::new(dates, mvs, pls).unwrap();

        let twrr_dates = Rtn::dates(1, 5).unwrap();
        let twrr_cr = rtn.twrr_cr(1, 5).unwrap();
        assert_eq!(twrr_dates, vec![1, 2, 3, 4, 5]);
        assert_eq!(twrr_cr, vec![None, None, None, None, None]);
        assert_near_eq!(rtn.mvs, vec![100., 100., 102., 103., 104.]);
        assert_near_eq!(rtn.pls, vec![0., 0., 2., 1., 1.]);

        let twrr_dates = Rtn::dates(2, 5).unwrap();
        let twrr_dr = rtn.twrr_dr(2, 5).unwrap();
        assert_eq!(twrr_dates, vec![2, 3, 4, 5]);
        assert_near_eq!(
            twrr_dr,
            vec![Some(0.0), Some(0.02), Some(1. / 102.), Some(1. / 103.)]
        );
        let twrr_cr = rtn.twrr_cr(2, 5).unwrap();
        assert_near_eq!(twrr_cr, vec![Some(0.0), Some(0.02), Some(0.03), Some(0.04)]);
    }
    #[test]
    fn dietz_ok() {
        let dates = vec![0, 5, 6, 7];
        let mvs = vec![100., 102., 103., 104.];
        let pls = vec![0., 2., 1., 1.];
        let rtn = Rtn::new(dates, mvs, pls).unwrap();
        let avc = rtn.dietz_avc(4, 7).unwrap();
        let dietz = rtn.dietz(4, 7).unwrap();
        assert_near_eq!(avc, vec![Some(100.), Some(100.), Some(100.), Some(100.)]);
        assert_near_eq!(dietz, vec![Some(0.0), Some(0.02), Some(0.03), Some(0.04)]);
    }
    #[test]
    fn dietz_ok2() {
        let dates = vec![0, 50, 100];
        let mvs = vec![100., 205., 305.];
        let pls = vec![0., 5., 0.];
        let rtn = Rtn::new(dates, mvs, pls).unwrap();

        let avc = rtn.dietz_avc(1, 100).unwrap();
        let avc_n: Option<f64> = *avc.last().unwrap();
        assert_eq!(avc_n.unwrap(), 100. + 100. * 51. / 100. + 100. * 1. / 100.);
        let dietz = rtn.dietz(1, 50).unwrap();
        let dietz_n: Option<f64> = *dietz.last().unwrap();
        assert_eq!(dietz_n.unwrap(), 5. / (100. + 100. / 50.));

        let avc = rtn.dietz_avc(51, 100).unwrap();
        let avc_n: Option<f64> = *avc.last().unwrap();
        assert_eq!(avc_n.unwrap(), 205. + 100. * 1. / 50.);
        let dietz = rtn.dietz(51, 100).unwrap();
        let dietz_n: Option<f64> = *dietz.last().unwrap();
        assert_eq!(dietz_n.unwrap(), 0.);
    }
    #[test]
    fn ok_with_zero_begin() {
        let dates = vec![-1, 0, 1, 2, 50, 100];
        let mvs = vec![0., 0., 110., 50., 205., 305.];
        let pls = vec![0., 0., 10., 5., 0., 0.];
        let rtn = Rtn::new(dates, mvs, pls).unwrap();
        assert_eq!(rtn.cf(2).unwrap(), 100.);
        let avc = rtn.dietz_avc(1, 1).unwrap();
        assert_eq!(avc, vec![Some(100.)]);
        let dietz = rtn.dietz(0, 0).unwrap();
        assert_eq!(dietz, vec![None]);
        let dietz = rtn.dietz(1, 1).unwrap();
        assert_eq!(dietz, vec![Some(0.1)]);
        let twdr = rtn.twrr_dr(0, 1).unwrap();
        assert_eq!(twdr, vec![None, Some(0.1)]);
        let avc = rtn.dietz_avc(2, 2).unwrap();
        assert_eq!(avc, vec![Some(110.)]);
    }
}
