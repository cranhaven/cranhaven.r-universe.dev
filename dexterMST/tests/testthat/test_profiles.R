context('profiles')
library(dplyr)
library(RSQLite)

on_cran = function() !interactive() && !isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))

test_that('profiles match simulated rasch data', {
  if(on_cran())
    RcppArmadillo::armadillo_throttle_cores(1)
  set.seed(123)
  
  items = data.frame(item_id=sprintf("item%02i",1:70), item_score=1, delta=sort(runif(70,-1,1)))
  
  design = data.frame(item_id=sprintf("item%02i",1:70),
                      module_id=rep(c('M4','M2','M5','M1','M6','M3', 'M7'),each=10))
  
  scoring_rules = data.frame(
    item_id = rep(items$item_id,2), 
    item_score= rep(0:1,each=nrow(items)),
    response= rep(0:1,each=nrow(items))) # dummy respons
  
  rall =  mst_rules(
    b1 = M1[0:5] --+ M2[0:10] --+ M4,
    b2a = M1[0:5] --+ M2[11:15] --+ M5[11:20] --+ M4,
    b2b = M1[0:5] --+ M2[11:15] --+ M5[21:25],
    b3a = M1[6:10] --+ M3[6:15] --+ M6[6:20] --+ M5,
    b3b = M1[6:10] --+ M3[6:15] --+ M6[21:Inf],
    b4 = M1[6:10] --+ M3[16:20] --+ M7)

  
  rlast =  mst_rules(
    b1 = M1[0:5] --+ M2[0:5] --+ M4 , 
    b2a = M1[0:5] --+ M2[6:10] --+ M5[0:5] --+ M4,
    b2b = M1[0:5] --+ M2[6:10] --+ M5[6:Inf],
    b3a = M1[6:10] --+ M3[0:5] --+ M6[0:5] --+ M5,
    b3b = M1[6:10] --+ M3[0:5] --+ M6[6:Inf],
    b4 = M1[6:10] --+ M3[6:10] --+ M7)
  

  dat = sim_mst(items, rnorm(3000), design, rall,'all')
  dat$test_id='sim_all'
  dat$response=dat$item_score
  
  db = create_mst_project(":memory:")
  add_scoring_rules_mst(db, scoring_rules)
  
  create_mst_test(db,
                  test_design = design,
                  routing_rules = rall,
                  test_id = 'sim_all',
                  routing = "all")
  
  add_response_data_mst(db, dat)
  
  dat = sim_mst(items, rnorm(3000), design, rlast,'last')
  dat$person_id = dat$person_id+1e5
  dat$test_id='sim_last'
  dat$response=dat$item_score
  
  create_mst_test(db,
                  test_design = design,
                  routing_rules = rlast,
                  test_id = 'sim_last',
                  routing = "last")
  
  add_response_data_mst(db, dat)
  
  f = fit_enorm_mst(db)
  
  # make somewhat skewed domains
  domains = data.frame(item_id=sprintf("item%02i",1:70),
                      cat=sample(letters[1:4],70,replace=TRUE,prob=c(1,1.5,2,1)))
  
  pt = profile_tables_mst(f,domains,'cat')
  
  tst_sum = pt %>%
    group_by(test_id,booklet_id,booklet_score) %>%
    summarise(ss=sum(expected_domain_score))
  
  expect_lt(max(abs(tst_sum$ss-tst_sum$booklet_score)),1e-8)
  
  manifest = get_responses_mst(db) %>%
    inner_join(domains, by='item_id') %>%
    group_by(person_id,test_id,booklet_id,cat) %>%
    summarise(domain_score=sum(item_score)) %>%
    mutate(booklet_score = sum(domain_score)) %>%
    ungroup() %>%
    group_by(test_id,booklet_id, booklet_score, cat) %>%
    summarise(m=mean(domain_score),n=n())
  
  tst = inner_join(pt,manifest, by=c('test_id','booklet_id','booklet_score','cat')) 

  #plot(tst$expected_domain_score,tst$m,cex=200*tst$n/sum(tst$n),pch=19)  
  expect_lt(weighted.mean((tst$expected_domain_score-tst$m)^2,tst$n),.06)
  dbDisconnect(db)
  
  if(on_cran())
    RcppArmadillo::armadillo_reset_cores()
})