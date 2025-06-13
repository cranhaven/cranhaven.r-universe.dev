createCLangRoot <- function(rootStates) {

  head <- '\n\nvoid myroot(int *neq, double *t, double *y, int *ng, double *gout,double *out, int *ip ){\n\n'

  stateId <- which(rootStates > 0)

  body <- paste0('\tgout[', 0:(sum(rootStates) - 1), '] = y[', stateId - 1, '];')
  formatedBody <- paste0(body, collapse = '\n')

  rootFunction <- paste0(head, formatedBody, '\n}')

  return(rootFunction)

}

createRoot <- function(rootStates) {

  rootFunction <- 'function(t,x,param) x'

  return(rootFunction)
}

createEvent <- function(tolerance, value) {

  head <- 'function(t,x,param) {\n\n'

  body <- paste0('\t x[which(x <= ', tolerance, ')] = ', value)

  eventFunction <- paste0(head, body, '\n\n\treturn(x)\n}')

  return(eventFunction)

}
