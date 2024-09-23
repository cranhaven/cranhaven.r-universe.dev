genDefine <- function() {

  mod1 <-rxode2parse("
    C2 = centr/V2
    C3 = peri/V3
    d/dt(depot) =-KA*depot
    alag(depot) = 3
    d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3
    d/dt(peri)  =                    Q*C2 - Q*C3
    d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff
  ")

  mod <- rxode2parse("
a = 6
b = 0.6
d/dt(intestine) = -a*intestine
d/dt(blood)     = a*intestine - b*blood
")

  mv <- mod1

  .ctl <- rxode2::rxControl()

  .n <- gsub("[.]","_",names(.ctl))
  sink(devtools::package_file("inst/include/rxode2parse_control.h")) # nolint
  cat("#pragma once\n")
  cat("#ifndef __rxode2parse_control_H__\n#define __rxode2parse_control_H__\n")
  cat(paste(paste0("#define ", "Rxc_", .n, " ", seq_along(.n)-1),collapse="\n"))

  .mv <- mod1

  .nmv <- gsub("[.]", "_", names(.mv))
  cat("\n")
  cat(paste(paste0("#define RxMv_", .nmv, " ", seq_along(.nmv)-1),collapse="\n"))
  .nmvf <- names(.mv$flag)
  cat("\n")
  cat(paste(paste0("#define RxMvFlag_", .nmvf, " ", seq_along(.nmvf)-1),collapse="\n"))
  cat("\n")

  .nmvt <- gsub("[.]", "_", names(.mv$trans))

  cat("\n")
  cat(paste(paste0("#define RxMvTrans_", .nmvt, " ",
                   seq_along(.nmvt)-1),collapse="\n"))
  cat("\n")

  et <- structure(list(time = c(0, 0.05, 0.1, 0.2, 0.3, 0.5), cmt = c("(default)", "(obs)", "intestine", "-intestine", "intestine", "out"), amt = c(0.0833333333333333, NA, 3, NA, 3, 3), rate = c(2, 0, 0, 0, 0, 0), ii = c(1, 0, 3, 0, 3, 0), addl = c(9L, 0L, 0L, 0L, 0L, 0L), evid = c(1L, 2L, 1L, 2L, 1L, 1L), ss = c(0L, 0L, 1L, 0L, 2L, 0L)), class = "data.frame", row.names = c(NA, -6L))

  ett1 <- etTransParse(et, mod, keepDosingOnly=TRUE)
  .n <- gsub("[.]", "_", names(attr(class(ett1), ".rxode2")))

  cat(paste(paste0("#define RxTrans_", .n, " ", seq_along(.n)-1),collapse="\n"))
  cat(paste0("\n#define RxTransNames CharacterVector _en(", length(.n), ");",
             paste(paste0("_en[",seq_along(.n)-1,']="', .n, '";'), collapse=""),"e.names() = _en;"))
  cat("\n")
  cat("\n#endif // __rxode2parse_control_H__\n")
  sink() # nolint
}


if (requireNamespace("devtools", quietly = TRUE)) {
  .var <- deparse(rxode2::rxSupportedFuns())
  .num <- deparse(rxode2:::.rxSEeq)
  .var[1] <- paste0(".parseEnv$.parseFuns <- ", .var[1])
  .num[1] <- paste0(".parseEnv$.parseNum <- ", .num[1])
  .pf <- devtools::package_file("R/parseFuns.R")
  unlink(.pf)
  parseFuns.R <- file(.pf, "wb")
  writeLines(.var, parseFuns.R)
  writeLines(.num, parseFuns.R)
  close(parseFuns.R)

  message("rebuild rxode2parse_control.h")
  genDefine()
  message("done")
}
