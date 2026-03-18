# plotBart 0.1.7
- Documentation updates for CRAN

# plotBart 0.1.6
- Documentation updates for CRAN

# plotBart 0.1.5
- Documentation is now included for each function

# plotBart 0.1.4
- propensity_scores() no longer exported
- Removed legend argument from `plot_moderator_c_pd()`. Legend can be modified using `p + theme(legend.position = "<position>")`
- `plot_overlap_pScores()` and `propensity_scores()` now pass additional arguments to `bartCause::bartc()` which enables seed setting

# plotBart 0.1.3
- Homogenized function argument names across functions. This is code breaking as previous argument names are not supported.
- `plot_moderator_search()` now returns a ggplot object instead of an `rpart.plot` object

# plotBart 0.1.2
- Fixed bug in waterfall plot

# plotBart 0.1.1
- Fixed bug in pate plot

# plotBart 0.1.0
- Initial release

