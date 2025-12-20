.onLoad <- function(...)
{
  registerS3method("knit_print", "math", .knit_print.math)
}

.onAttach <- function(libname, pkgname)
{
  if(!requireNamespace("rolog", quietly=TRUE))
    stop("Could not load R package rolog.")

  if(!rolog::rolog_ok())
    stop("Could not attach R package rolog.")

  rolog::consult(system.file("pl/mathml.pl", package=pkgname))
}

#' MathML output
#'
#' @md
#' 
#' @param term
#' an R call or symbol/number. This function translates _term_ into a
#' MathML string.
#'
#' @param flags (default NULL)
#' list of flags that control the translation. This includes "context" settings
#' such as error("ignore"), or a default number of decimal places for numeric
#' output.
#'
#' @param env (default globalenv())
#' The R environment in which r_eval is being executed.
#' 
#' @return
#' A string with the MathML representation of _term_.
#'
#' @seealso [mathjax()]
#'
#' @details In some functions, the Prolog code may ring back R, for example, to
#' find the names of function arguments. For example (see vignette), when
#' rendering the call `integrate(g, lower=0L, upper=Inf)` as Int_0^Inf g(x) dx,
#' Prolog needs to know that the function g is a function of x. The Prolog rule
#' then searches for the formalArgs of g in the environment _env_.
#'
#' @examples
#' mathml(term=quote((a + b)^2L == a^2L + 2L*a*b + b^2L))
#' mathml(term=3.14159265, flags=list(round=3L))
#' mathml(term=3.14159265, flags=list(quote(round(3L))))
#'
mathml <- function(term=quote((a + b)^2L == a^2L + 2L*a*b + b^2L), flags=NULL,
  env=globalenv())
{
  flags <- c(flags, list(cat=FALSE))
  t <- rolog::once(call("r2mathml", term, expression(X), flags),
    options=list(preproc=list(rolog::preproc, mathml_preproc)),
    env=env)
  r <- paste(t$X, collapse="")
  if(flags$cat)
    return(cat(r))

  return(r)
}

# Prolog representation of not equal etc. (left: R, right: Prolog)
.mathml_operators = c(
  "%%" = "mod",
  "%/%" = "div")

#' Map R operators to their respective Prolog counterparts
#'
#' @param query
#' an R call or symbol/number. This function translates components of _query_
#' into their respective counterparts from Prolog
#'
#' @return
#' The translated query
#'
#' @md
#'
#' @seealso [mathjax()], [mathml()]
#'
#' @examples
#' mathml_preproc(quote(5 %% 2))
#'
mathml_preproc <- function(query=quote(5 %% 2))
{
  if(is.call(query))
  {
    args <- as.list(query)
    index <- (args[[1]] == names(.mathml_operators))
    if(any(index))
      args[[1]] <- as.symbol(.mathml_operators[index])

    args[-1] <- lapply(args[-1], FUN=mathml_preproc)
    return(as.call(args))
  }

  if(is.list(query))
    query[] <- lapply(query, FUN=mathml_preproc)

  if(is.function(query))
    body(query) <- mathml_preproc(body(query))

  return(query)
}

#' Mathjax output
#'
#' @param term
#' an R call or symbol/number. This function translates _term_ into a
#' LaTeX/MathJax string.
#'
#' @param flags (default NULL)
#' list of flags that control the translation
#'
#' @param env (default globalenv())
#' The R environment in which r_eval is being executed (see vignette for
#' details, "Ringing back to R").
#'
#' @return
#' A string with the MathJax representation of _term_.
#'
#' @md
#'
#' @details In some functions, the Prolog code may ring back R, for example, to
#' find the names of function arguments. For example (see vignette), when
#' rendering the call `integrate(g, lower=0L, upper=Inf)` as Int_0^Inf g(x) dx,
#' Prolog needs to know that the function g is a function of x. The Prolog rule
#' then searches for the formalArgs of g in the environment _env_.
#'
#' @seealso [mathml()]
#'
#' @examples
#' mathjax(term=quote((a + b)^2L == a^2L + 2L*a*b + b^2L))
#'
mathjax <- function(term=quote((a + b)^2L == a^2L + 2L*a*b + b^2L), flags=NULL,
  env=globalenv())
{
  flags <- c(flags, list(cat=FALSE))
  t <- rolog::once(call("r2mathjax", term, expression(X), flags),
    options=list(preproc=list(rolog::preproc, mathml_preproc)),
    env=env)
  r <- paste(t$X, collapse="")
  if(flags$cat)
    return(cat(r))

  return(r)
}

#' MathML or MathJax output, depending on the knitr context
#'
#' @param term
#' an R call or symbol/number. This function translates _term_ into a
#' LaTeX/MathJax string.
#'
#' @param flags (default NULL)
#' list of flags that control the translation
#'
#' @param env (default parent.frame())
#' The R environment in which r_eval is being executed (see vignette for
#' details, "Ringing back to R").
#'
#' @return
#' A string with the MathML or MathJax representation of _term_.
#'
#' @md
#'
#' @details This function checks knitr::is_html_output() 
#' and knitr::is_html_output() and invokes the respective function mathml() or
#' mathjax(). Outside of knitr context, MathML is returned, and a warning is
#' given.
#'
#' @seealso [mathml()], [mathjax()], [inline()]
#'
#' @examples
#' mathout(term=quote((a + b)^2L == a^2L + 2L*a*b + b^2L))
#'
#' inline(term=quote((a + b)^2L == a^2L + 2L*a*b + b^2L))
#'
mathout <- function(term, flags=NULL, env=parent.frame())
{
  if(knitr::is_html_output())
    return(mathml(term, flags=c(flags, list(cat=TRUE)), env=env))

  if(knitr::is_latex_output())
    return(mathjax(term, flags=c(flags, list(cat=TRUE)), env=env))

  if(knitr::pandoc_to("docx"))
    return(mathjax(term, flags=c(flags, list(cat=TRUE)), env=env))

  warning("knitr output not specified. Use mathml() or mathjax() outside of code chunks.")  
  mathjax(term, flags=c(flags, list(cat=TRUE)), env=env)
}

#' @rdname mathout
#' @export
inline <- function(term, flags=NULL, env=parent.frame())
{
  mathout(term, flags=c(flags, list(cat=FALSE)), env=env)
}

#' Adds the class "math" to the object for knitr output via `mathout()`
#'
#' @param term
#' an R call or symbol/number. This function translates _term_ into a
#' LaTeX/MathJax string.
#'
#' @param flags (default NULL)
#' list of flags that control the translation
#'
#' @return
#' _term_ with additional class "math" and _flags_ as attributes.
#'
#' @md
#'
#' @seealso [mathml()], [mathjax()], [mathout()]
#'
#' @examples
#' math(term=quote((a + b)^2L == a^2L + 2L*a*b + b^2L))
math <- function(term, flags=NULL)
{
  class(term) <- c("math", class(term))
  attr(term, "flags") <- flags
  return(term)
}
  
.knit_print.math <- function(x, options, inline=FALSE) 
{
  flags <- attr(x, "flags")
  if(inline)
    return(knitr::asis_output(as.character(inline(x, flags=flags))))

  structure(knitr::asis_output(as.character(mathout(x, flags=c(cat=FALSE, flags)))))
}

#' Add a name attribute to an element (most often, an R function)
#'
#' @param x
#' an R object, e.g., an R function
#'
#' @param name
#' the name of the object/function
#'
#' @return
#' The object with the name attribute
#'
#' @md
#'
#' @examples
#' f <- function(x) {sin(x)}
#' mathjax(call("integrate", name(f, "sin"), 0L, 2L*pi))
#'
name <- function(x, name)
{
  attributes(x)$name <- name
  return(x)
}

#' Calligraphic font
#'
#' @param x
#' an R symbol. This function is used to render the content in calligraphic font
#' in MathJax. In MathML, script font is used.
#'
#' @return
#' The function cal is a wrapper for the identity function.
#'
#' @md
#'
#' @seealso [identity()]
#'
#' @examples
#' mathjax(quote(K %in% cal(K)))
#'
cal <- identity

#' sum over a range. On the R side, this function just returns the first
#' argument, but allows for decorations.
#'
#' @param x
#' the object to be summed
#'
#' @param from
#' decoration for sum_from^to x_i
#'
#' @param to
#' decoration for sum_from^to x_i
#'
#' @return
#' The function returns sum(x)
#'
#' @md
#'
#' @seealso [sum()], [prod_over()]
#'
#' @examples
#' mathjax(quote(sum_over(x[i], i=1L, N)))
#'
sum_over <- function(x, from, to)
  sum(x)

#' product over a range. On the R side, this function just returns the product
#' of the first argument, but allows for decorations.
#'
#' @param x
#' the object to be multiplied
#'
#' @param from
#' decoration for prod_from^to x_i
#'
#' @param to
#' decoration for prod_from^to x_i
#'
#' @return
#' The function returns prod(x)
#'
#' @md
#'
#' @seealso [prod()], [sum_over()]
#'
#' @examples
#' mathjax(quote(prod_over(x[i], i=1L, N)))
#'
prod_over <- function(x, from, to)
  prod(x)

#' Canonicalize an R call: Reorder the function arguments
#'
#' @param term
#' an R call.
#'
#' @param drop
#' whether to drop the argument names or not
#'
#' @return
#' The R function, with arguments rearranged
#'
#' @md
#'
#' @examples
#' canonical(term=quote(`%in%`(table=Table, x=X)))
#'
canonical <- function(term=quote(`%in%`(table=Table, x=X)), drop=TRUE)
{
  attr <- attributes(term)

  if(is.call(term))
  {
    f <- match.fun(term[[1]])
    if(!is.primitive(f))
      term <- match.call(f, term)
    term[-1] <- lapply(term[-1], canonical, drop=drop)
  }

  if(drop)
    term <- unname(term)

  attributes(term) <- attr
  return(term)
}

#' Hook for custom symbols
#' 
#' hook(term, display)
#' hook_fn(fn)
#' unhook(term)
#' hooked(term)
#'
#' @param term
#' an R call or symbol/number. This is the expression to replace.
#'
#' @param display
#' an R call or symbol/number. This is shown instead of _term_.
#'
#' @param quote (default is TRUE)
#' indicates that _term_ and _display_ should be quoted.
#'
#' @param as.rolog (default is TRUE)
#' indicates that simplified quasi-quotation is to be used.
#'
#' @param fn
#' a custom function. The name of _fn_ is replaced by its function body.
#' 
#' @return
#' hook and unhook return TRUE on success. hooked returns the hooked
#' expression or FALSE on failure.
#'
#' @md
#'
#' @examples
#' hook(t0, subscript(t, 0))
#' hooked(quote(t0))
#' mathml(quote(t0))
#' hook(term=quote(t0), display=quote(superscript(t, 0)), quote=FALSE)
#' mathml(quote(t0))
#' unhook(t0)
#' mathml(quote(t0))
#' square <- function(x) {x^2} ; hook_fn(square)
#'
hook <- function(term, display=NULL, quote=TRUE, as.rolog=TRUE)
{
  if(quote)
  {
    term <- substitute(term)
    display <- substitute(display)
  }
  
  if(as.rolog)
  {
    term <- rolog::as.rolog(term)
    display <- rolog::as.rolog(display)
  }

  r <- rolog::once(call("asserta", call("math_hook", term, display)))
  if(isFALSE(r))
    return(FALSE)

  invisible(r)
}

#' @rdname hook
#' @export
unhook <- function(term, quote=TRUE, as.rolog=TRUE)
{
  if(quote)
    term <- substitute(term)

  if(as.rolog)
    term <- rolog::as.rolog(term)

  r <- rolog::once(call("retractall", call("math_hook", term, expression(X))))
  if(isFALSE(r))
    return(FALSE)
  
  invisible(r)
}

#' @rdname hook
#' @export
hooked <- function(term)
{
  r <- rolog::once(call("math_hooked", term, expression(X)))
  if(isFALSE(r))
    return(FALSE)
  
  return(r$X)
}

#' @rdname hook
#' @export
hook_fn <- function(fn)
{
  name <- as.character(substitute(fn))
  args <- names(formals(fn))
  dotargs <- sprintf(".%s", args)
  subst <- lapply(dotargs, as.name)
  arg1 <- as.call(c(as.name(name), subst))
  names(subst) <- args
  arg2 <- do.call(substitute, list(body(fn), subst))
  eval(call("hook", arg1, arg2))
}

#' Multiplication
#'
#' @name dot
#'
#' @param e1
#' numerator
#'
#' @param e2
#' denominator
#'
#' @return
#' e1 * e2
#'
dot <- function(e1, e2)
  e1 * e2

#' @rdname dot
#' @export
nodot <- dot

#' @rdname dot
#' @export
times <- dot

#' Division displayed as fraction
#'
#' @param e1
#' numerator
#'
#' @param e2
#' denominator
#'
#' @return
#' e1 / e2
#'
frac <- function(e1, e2)
  e1 / e2


#' Division displayed as large fraction
#'
#' @param e1
#' numerator
#'
#' @param e2
#' denominator
#'
#' @return
#' e1 / e2
#'
#' @md
#' @seealso [frac()], [over()]
dfrac <- frac

#' Return function body
#'
#' @param fname
#' not clear
#'
#' @param body
#' not clear
#'
#' @return
#' body
#'
fname <- function(fname, body)
{
  return(body)
}

#' Plus Minus, it shows x and calculates x +- y
#'
#' @param x 
#' first term
#'
#' @param y 
#' second term
#'
#' @return c(x - y, x + y)
#' x plus min y
#'
'%+-%' <- function(x, y)
{
  c(x - y, x + y)
}


#' Product x * y, shown as x dot y
#'
#' @param x
#' first factor
#'
#' @param y
#' second factor
#'
#' @return
#' x * y 
#'
'%.%' <- function(x, y)
  x * y

#' Approximate equality, shown as x ~~ y
#'
#' @param x
#' first argument
#'
#' @param y
#' second argument
#'
#' @return
#' The result of isTRUE(all.equal(x, y))
#'
'%~~%' <- function(x, y)
  isTRUE(all.equal(x, y))

#' Equivalence, shown as x == y
#'
#' @param x 
#' first argument
#'
#' @param y 
#' second argument
#'
#' @return 
#' x == y
#'
'%==%' <- function(x, y)
  x == y


#' Congruence, shown as x =~ y
#'
#' @param x 
#' first argument
#'
#' @param y 
#' second argument
#'
#' @return 
#' x == y, e.g., a cong b
#'
'%=~%' <- function(x, y)
  x == y

#' Proportional, shown as x o< y
#'
#' @param x 
#' first argument
#'
#' @param y 
#' second argument
#' 
#' @return 
#' NA
#'
'%prop%' <- function(x, y)
  NA

#' Double sided arrow, presented as x <-> y
#'
#' @param x 
#' first element
#'
#' @param y 
#' second element
#'
#' @return 
#' NA, it produces a double sided arrow
#'
#'
'%<->%' <- function(x, y)
  NA

#' Right arrow, presented as x -> y
#'
#' @param x 
#' first element
#'
#' @param y 
#' second element
#'
#' @return 
#' NA, it produces a right arrow
#'
'%->%' <- function(x, y)
  NA

#' Left arrow, presented as x <- y
#'
#' @param x 
#' first element
#'
#' @param y 
#' second element
#'
#' @return 
#' NA, it produces a left arrow
#'
'%<-%' <- function(x, y)
  NA

#' Up arrow, presented as x up y
#'
#' @param x 
#' first element
#'
#' @param y 
#' second element
#'
#' @return 
#' NA, it produces an upward arrow
#'
'%up%' <- function(x, y)
  NA

#' Down arrow, presented as x downarrow y
#'
#' @param x 
#' first element
#'
#' @param y 
#' second element
#'
#' @return 
#' NA, it produces a downward arrow
#'
'%down%' <- function(x, y)
  NA

#' If and only if condition, displayed as x <=> y
#'
#' @param x 
#' first element
#'
#' @param y 
#' second element
#'
#' @return 
#' NA, it produces a double arrow double-sided
#'
'%<=>%' <- function(x, y)
  NA

#'Right double arrow, displayed as x => y
#'
#' @param x 
#' first element
#'
#' @param y 
#' second element
#'
#' @return 
#' NA, it produces a right double arrow
#'
'%<=%' <- function(x, y)
  NA

#' Left double arrow, displayed as x <= y
#'
#' @param x 
#' first element
#'
#' @param y 
#' second element
#'
#' @return 
#' NA, it produces a left double arrow
#'
'%=>%' <- function(x, y)
  NA

#' Up double arrow, displayed as x uArr y
#'
#' @param x 
#' first element
#'
#' @param y 
#' second element
#'
#' @return 
#' NA, it produces a upward double arrow
#'
'%dblup%' <- function(x, y)
  NA

#' Down double arrow, displayed as x dArr y
#'
#' @param x 
#' first element
#'
#' @param y 
#' second element
#'
#' @return 
#' NA, it produces a downward double arrow
#'
'%dbldown%' <- function(x, y)
  NA


#' denote
#' This is a function that allows the user to insert abbreviations in the formula,
#' explain them and make the needed computations
#'
#' @param abbr
#' Abbreviation used in the text to refer to the calculation, for example 's_p' for the pooled
#' variance.
#'
#' @param expr
#' Expression: calculations to be made in order to obtain the value to which the abbreviation
#' refers to.
#'
#' @param info
#' Information: Explanation of the formula used to provide the value of the abbreviation.
#' e.g. 'the pooled variance'
#'
#' @return expr
#' e.g., x denotes a^2 + b
#'
denote <- function(abbr, expr, info)
  return(expr)

#' omit_left
#' This is a function that allows the user to highlight the mistakes,
#' in particular the omissions in the left-hand side of the expression
#'
#' @param expr 
#' The expression, e.g. a + b
#'
#' @return substitute(expr)[[3]], e.g., b from a + b
#'
omit_left <- function(expr)
{
  # use third element of [-, A, B]
  eval(substitute(expr)[[3]])
}

#' omit_right
#' This is a function that allows the user to highlight the mistakes,
#' in particular the omissions in the right-hand side of the expression
#'
#' @param expr expression
#'
#' @return substitute(expr)[[2]], e.g., a from a + b
#'
omit_right <- function(expr)
{
  eval(substitute(expr)[[2]])
}

#' omit
#'
#' This is a function that allows the user to highlight the mistakes,
#' in particular the omission of an element from a list.
#'
#' @param expr expression
#'
#' @return NULL
#' e.g., remove a + b from a + b
#'
omit <- function(expr)
{
  NULL
}


#' add_left
#'
#' This is a function that allows the user to highlight the mistakes,
#' in particular the redundancies in the left-hand side of the expression.
#'
#' @param expr expression
#'
#' @return expr  e.g., highlights a + from a + b
#'
add_left <- function(expr)
{
  return(expr)
}

#' add_right
#'
#' This is a function that allows the user to highlight the mistakes,
#' in particular the redundancies in the right-hand side of the expression.
#'
#' @param expr expression
#'
#' @return expr , e.g., highlights + b from a + b
#'
add_right <- function(expr)
{
  return(expr)
}

#' add
#'
#' This is a function that allows the user to highlight the mistakes,
#' in particular an extra element in a list
#'
#' @param expr expression
#'
#' @return expr ,  e.g., highlights a + b from a + b
#'
add <- function(expr)
{
  return(expr)
}

#' instead
#'
#' This is a function that allows the user to highlight the mistakes,
#' in particular adds a curly bracket under the wrong term and it provides the
#' correct solutions.
#'
#' @param inst
#' the wrong term
#'
#' @param of
#' the correct term
#'
#' @return
#' inst
#'
#' @examples
#' 1 + instead(2, 3)
#'
#' @examples
#' mathml(term=quote(1 + instead(2, 3)))
#'
instead <- function(inst, of)
{
  return(inst)
}

#' Identity functions for different font styles
#'
#' @name fontstyles
#'
#' @param x
#' the expression to render
#'
#' @return
#' x
#'
#' @examples
#' plain(1) + bold(2) + italic(3)
#'
#' @examples
#' mathml(term=quote(plain(abc) + bold(def) + italic(ghi)))

#' @rdname fontstyles
#' @export
plain <- identity

#' @rdname fontstyles
#' @export
italic <- identity

#' @rdname fontstyles
#' @export
bold <- identity

#' Identity functions for different decorations
#'
#' @name decorations
#'
#' @param x
#' the expression to render
#'
#' @return
#' x
#'
#' @examples
#' roof(1) + mean(2) + boxed(3) + cancel(4) + phantom(5) + prime(6) + tilde(7)
#'
#' @examples
#' mathml(quote(roof(b) + mean(X) + boxed(3) + cancel(4) + phantom(5)))

#' @rdname decorations
#' @export
roof <- identity

#' @rdname decorations
#' @export
boxed <- identity

#' @rdname decorations
#' @export
cancel <- identity

#' @rdname decorations
#' @export
phantom <- identity

#' @rdname decorations
#' @export
prime <- identity

#' @rdname decorations
#' @export
tilde <- identity

#' @rdname decorations
#' @export
over <- identity

#' @rdname decorations
#' @export
under <- identity

#' @rdname decorations
#' @export
underover <- identity

#' @rdname decorations
#' @export
hyph<- identity

#' @rdname decorations
#' @export
color<- identity
