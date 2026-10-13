# Simulate a user interrupt without depending on rlang. tryCatch()'s
# `interrupt =` handler is an exiting handler, so signalling this condition
# unwinds execution just like a real interrupt would.
simulate_interrupt <- function() {
    signalCondition(
        structure(
            class = c("interrupt", "condition"),
            list(message = "", call = sys.call(-1))
        )
    )
}
