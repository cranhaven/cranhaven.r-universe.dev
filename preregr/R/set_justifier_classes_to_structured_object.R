set_justifier_classes_to_structured_object <- function(x) {

  res <- x;

  ### Sources

  if (is.null(res$sources) || (length(res$sources) == 0)) {
    res$sources <- structure(
      list(),
      class = c("justifier", "justifierStructured", "justifierSource", "list")
    );
  } else {
    res$sources <-
      lapply(res$sources,
             `class<-`,
             c("justifierSource",
               "singleJustifierElement",
               "justifierElement",
               "justifier"));
    class(res$sources) <- c("justifier", "justifierStructured", "justifierSource", "list");
  }

  ### Assertions

  if (is.null(res$assertions) || (length(res$assertions) == 0)) {
    res$assertions <- structure(
      list(),
      class = c("justifier", "justifierStructured", "justifierAssertion", "list")
    );
  } else {
    res$assertions <-
      lapply(
        res$assertions,
        replace_empty_lists_with_null,
        select = "source"
      );
    res$assertions <-
      lapply(
        res$assertions,
        set_class_selectively,
        select = "source",
        class = c("justifier", "justifierIdRef", "justifierSource", "character")
      );
    res$assertions <-
      lapply(res$assertions,
             `class<-`,
             c("justifierAssertion", "singleJustifierElement", "justifierElement", "justifier"));
    class(res$assertions) <- c("justifier", "justifierStructured", "justifierAssertion", "list");
  }

  ### Justifications

  if (is.null(res$justifications) || (length(res$justifications) == 0)) {
    res$justifications <- structure(
      list(),
      class = c("justifier", "justifierStructured", "justifierJustification", "list")
    );
  } else {
    res$justifications <-
      lapply(
        res$justifications,
        replace_empty_lists_with_null,
        select = "assertion"
      );
    res$justifications <-
      lapply(
        res$justifications,
        set_class_selectively,
        select = "assertion",
        class = c("justifier", "justifierIdRef", "justifierAssertion", "character")
      );
    res$justifications <-
      lapply(res$justifications,
             `class<-`,
             c("justifierJustification", "singleJustifierElement", "justifierElement", "justifier"));
    class(res$justifications) <- c("justifier", "justifierStructured", "justifierJustification", "list");
  }

  ### Decisions

  if (is.null(res$decisions) || (length(res$decisions) == 0)) {
    res$decisions <- structure(
      list(),
      class = c("justifier", "justifierStructured", "justifierDecision", "list")
    );
  } else {
    res$decisions <-
      lapply(
        res$decisions,
        replace_empty_lists_with_null,
        select = "justification"
      );
    res$decisions <-
      lapply(
        res$decisions,
        set_class_selectively,
        select = "justification",
        class = c("justifier", "justifierIdRef", "justifierJustification", "character")
      );
    res$decisions <-
      lapply(res$decisions,
             `class<-`,
             c("justifierDecision",
               "singleJustifierElement",
               "justifierElement",
               "justifier"));
    class(res$decisions) <- c("justifier", "justifierStructured", "justifierDecision", "list");
  }

  ### Justifier config

  if (is.null(res$justifier)) {
    res$justifier <- structure(
      list(),
      class = c("justifier", "justifierStructured", "justifierJustifier", "list")
    );
  } else {
    res$justifier <-
      lapply(res$justifier,
             `class<-`,
             c("justifierJustifier", "singleJustifierElement", "justifierElement", "justifier"));
    class(res$justifier) <- c("justifier", "justifierStructured", "justifierJustifier", "list");
  }

  class(res) <-
    c("justifier", "justifierStructuredObject", "list");

  return(res);

}

replace_empty_lists_with_null <- function(x,
                                          select) {

  if (is.null(x) || (length(x) == 0)) {
    return(x);
  }

  ### With thanks to Brenton Wiernik,
  ### https://twitter.com/bmwiernik/status/1436634785586323464

  if (is.list(x[[select]]) &&
      (length(x[[select]]) == 0)) {
    x[select] <- list(NULL);
  }

  return(x);

}

set_class_selectively <- function(x,
                                  select,
                                  class) {

  if (is.null(x) || (length(x) == 0)) {
    return(x);
  }

  if (is.character(x[[select]]) &&
      (length(x[[select]]) > 0)) {
    class(x[[select]]) <- class;
  }

  return(x);

}
