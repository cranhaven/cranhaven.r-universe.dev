### Example Workflow

# simulate tree using treesim

# simulate a tree using ape
tips = 8
t = ape::rtree(tips)
plot(t)
# simulate fossils using fossilsim
rate = 2
f = sim.fossils.poisson(rate, t)
# plot the complete output
plot(f, t)
# plot the reconstructed output
plot(f, t, reconstructed = TRUE)



## use Marios R package
# has uncorrelated and correlated characters



# Mk. done
# V. done
# G/lognormal?   half done
# Partitioning  done
# non-applicable
# missing (percentages)
# link gamma yes but added function to amalgamate morpho objects
# ordered
# correlated characters



plot(0, 0, xlim = c(0, 10), ylim = c(0, 1), type = "n")
curve(dgamma(x, 1), from = 0, to = 10,lwd = 2 ,add = TRUE, col = "forestgreen")

breakpoints <- qgamma(seq(0, 1, length.out = 5), shape = 1)

# Add vertical lines at the breakpoints
abline(v = breakpoints, col = "skyblue", lty = 2, lwd = 2)


# Add ticks at the mean value of each part
axis(3, at = unique(test$ACRV_rate), labels = round(unique(test$ACRV_rate), 4), col = "goldenrod3",
     col.axis = "goldenrod3", las = 2)

# Annotate the means
text(unique(test$ACRV_rate), 0.02, labels = round(unique(test$ACRV_rate), 2), col = "goldenrod3", pos = 3, las=1)





