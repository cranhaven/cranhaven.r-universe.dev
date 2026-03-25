
label <- function(net, transition) {

	net %>%
		transitions %>%
		filter(id == transition) %>%
		pull(label)
}
