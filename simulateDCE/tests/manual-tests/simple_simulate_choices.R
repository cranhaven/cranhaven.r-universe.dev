#–– 1) small toy dataset & simple utility for testing ––#
df_small <- data.frame(
  ID      = rep(1:5, each = 2),
  price   = rep(c(10, 20), 5),
  quality = rep(c(1,  2), 5)
)

beta <- list(
  bprice   = -1,
  bquality =  2
)

# note: use V.1 / V.2 so that rename_with("\\.", "_") => V_1/V_2
ut <- list(
  u1 = list(
    v1 = V.1 ~ bprice * price + bquality * quality,
    v2 = V.2 ~ 0
  )
)

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––#


simulate_choices(
    data = df_small,
    utility = ut,
    setspp = 2,
    bcoeff = beta
  )



### with decision groups

simulate_choices(
  data = df_small,
  utility = list(
    u1 = list(
      v1 = V.1 ~ bprice * price + bquality * quality,
      v2 = V.2 ~ 0
    ),
    u2 = list(
      v1 = V.1 ~ bprice * price + bquality * quality * 2,  # altered for group 2
      v2 = V.2 ~ 0
    )
  ),
  setspp = 2,
  bcoeff = beta,
  decisiongroups = c(0, 0.5, 1)
)
