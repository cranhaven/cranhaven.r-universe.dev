#![macro_use]

#[macro_export]
macro_rules! check_len {
    ( $x0:expr, $( $x:expr ),* ) => {
        $(
            let n = $x0.len();
            assert!(
                $x.len() == n, "the length of `{}` ({}) != the length of `{}` ({})",
                stringify!($x), $x.len(), stringify!($x0), n
            );
        )*
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn len_ok() {
        let aaa = vec![1; 3];
        let bbb = vec![2; 3];
        let ccc = vec![3; 3];
        check_len!(aaa, bbb);
        check_len!(aaa, bbb, ccc);
    }
    #[test]
    #[should_panic(expected = "the length of")]
    fn len_fail() {
        let aaa = vec![1; 3];
        let bbb = vec![2; 4];
        check_len!(aaa, bbb);
    }
    #[test]
    #[should_panic(expected = "the length of")]
    fn len_fail2() {
        let aaa = vec![1; 3];
        let bbb = vec![2; 3];
        let ccc = vec![3; 4];
        check_len!(aaa, bbb, ccc);
    }
}
