pc.test = function(mod,
                   data = list(),
                   split,
                   sig.level = 0.05,
                   details = FALSE,
                   hyp = TRUE){

if (!inherits(mod, "formula")){ # if mod is a fitted lm object ...
  X = model.matrix(terms(mod), model.frame(mod))
  y = model.response(model.frame(mod))
} else { # if mod is a formula...
  X = model.matrix(mod, data = data)
  y = model.response(model.frame(mod, data = data))
}

k = ncol(X) # Number of coefs in the model
n = nrow(X) # Number of observations

phase2 = n - split

SSR1 = sum(lm.fit(X[1:split,],y[1:split])$residuals^2)
SSR = sum(lm.fit(X[1:(split + phase2),],y[1:(split + phase2)])$residuals^2)

if (hyp){
  H = c(paste("No immediate break after t =", split), paste("Immediate break after t =", split))
  names(H) = c("H0:", "H1:")
  H = t(H)
} else {
  H = NULL
}

# F-Value
f.val = (1/phase2 * (SSR - SSR1)) / (1/(split - k) * SSR1)
p.val = 1 - pf(f.val, phase2, split - k)
f.crit = qf(1 - sig.level, phase2, split - k)

## Generate other data
test.result = if (p.val < sig.level) "rejected" else "not rejected"
results = data.frame(f.value = f.val,
                     crit.value = f.crit,
                     p.value = p.val,
                     sig.level = sig.level,
                     H0 = test.result,
                     row.names = "")

out = list()
attr(out, "title") = "Prognostic Chow test on structural break"
out$hyp = H # Null and alternative hypothesis
out$results = results # Basic test results
out$SSR1 = SSR1
out$SSR = SSR
out$periods1 = split
out$periods.total = n
out$nulldist = list(type = "f", df = c(phase2, split - k))

attr(out, "direction") = "right"
attr(out, "details") = if (details) {T} else {F}
attr(out, "type") = "htest"
attr(out, "test.type") = "pctest"
class(out) = c("desk")

return(out)
}
