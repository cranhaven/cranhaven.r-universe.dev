#' @title trioCheckFast
#' @description Check for Mendelian errors in trios
#' @param ffa father's alleles
#' @param mmo mother's alleles
#' @param oof offspring's alleles
#' @return TRUE if there is a Mendelian error
#' @export
trioCheckFast <- function(ffa, mmo, oof) {
        even <- 2 * seq_len(length(ffa)/2)
        odd <- even - 1
        fa_odd  <- ffa[odd]
        fa_even <- ffa[even]
        mo_odd  <- mmo[odd]
        mo_even <- mmo[even]
        of_odd <- oof[odd]
        of_even <- oof[even]
        fa0 <- (fa_odd == 0 | fa_even == 0)
        mo0 <- (mo_odd == 0 | mo_even == 0)
        of_odd0 <- (of_odd == 0)
        of_even0 <- (of_even == 0)
        ff1 <- (fa0 | of_odd0 | of_odd == fa_odd | of_odd == fa_even)
        ff2 <- (fa0 | of_even0 | of_even == fa_odd | of_even == fa_even)
        mm1 <- (mo0 | of_odd0 | of_odd == mo_odd | of_odd == mo_even)
        mm2 <- (mo0 | of_even0 | of_even == mo_odd | of_even == mo_even)
        (ff1 & mm2) | (ff2 & mm1)
}