## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  fig.width=6,
  fig.height=6)
data.table::setDTthreads(1)
## output: rmarkdown::html_vignette above creates html where figures are limited to 700px wide.
## Above CSS from https://stackoverflow.com/questions/34906002/increase-width-of-entire-html-rmarkdown-output main-container is for html_document, body is for html_vignette
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
N <- 300
library(data.table)
set.seed(1)
abs.x <- 2
reg.dt <- data.table(
  x=runif(N, -abs.x, abs.x),
  person=rep(1:2, each=0.5*N))
reg.pattern.list <- list(
  easy=function(x, person)x^2,
  impossible=function(x, person)(x^2+person*3)*(-1)^person)
reg.task.list <- list()
for(task_id in names(reg.pattern.list)){
  f <- reg.pattern.list[[task_id]]
  yname <- paste0("y_",task_id)
  reg.dt[, (yname) := f(x,person)+rnorm(N)][]
  task.dt <- reg.dt[, c("x","person",yname), with=FALSE]
  reg.task.list[[task_id]] <- mlr3::TaskRegr$new(
    task_id, task.dt, target=yname
  )$set_col_roles("person",c("group","stratum"))
}
reg.dt

## -----------------------------------------------------------------------------
(reg.tall <- nc::capture_melt_single(
  reg.dt,
  task_id="easy|impossible",
  value.name="y"))

## -----------------------------------------------------------------------------
if(require(animint2)){
  ggplot()+
    geom_point(aes(
      x, y),
      data=reg.tall)+
    facet_grid(
      task_id ~ person,
      labeller=label_both,
      space="free",
      scales="free")+
    scale_y_continuous(
      breaks=seq(-100, 100, by=2))
}

## -----------------------------------------------------------------------------
(reg_same_other <- mlr3resampling::ResamplingSameOtherCV$new())

## -----------------------------------------------------------------------------
(reg.learner.list <- list(
  if(requireNamespace("rpart"))mlr3::LearnerRegrRpart$new(),
  mlr3::LearnerRegrFeatureless$new()))

## -----------------------------------------------------------------------------
(reg.bench.grid <- mlr3::benchmark_grid(
  reg.task.list,
  reg.learner.list,
  reg_same_other))

## -----------------------------------------------------------------------------
if(FALSE){#for CRAN.
  if(require(future))plan("multisession")
}
if(require(lgr))get_logger("mlr3")$set_threshold("warn")
(reg.bench.result <- mlr3::benchmark(
  reg.bench.grid, store_models = TRUE))

## -----------------------------------------------------------------------------
reg.bench.score <- mlr3resampling::score(reg.bench.result)
reg.bench.score[1]

## -----------------------------------------------------------------------------
if(require(animint2)){
  ggplot()+
    scale_x_log10()+
    geom_point(aes(
      regr.mse, train.groups, color=algorithm),
      shape=1,
      data=reg.bench.score)+
    facet_grid(
      task_id ~ person,
      labeller=label_both,
      scales="free")
}

## ----SimulationsAnimintRegression---------------------------------------------
inst <- reg.bench.score$resampling[[1]]$instance
rect.expand <- 0.2
grid.dt <- data.table(x=seq(-abs.x, abs.x, l=101), y=0)
grid.task <- mlr3::TaskRegr$new("grid", grid.dt, target="y")
pred.dt.list <- list()
point.dt.list <- list()
for(score.i in 1:nrow(reg.bench.score)){
  reg.bench.row <- reg.bench.score[score.i]
  task.dt <- data.table(
    reg.bench.row$task[[1]]$data(),
    reg.bench.row$resampling[[1]]$instance$id.dt)
  names(task.dt)[1] <- "y"
  set.ids <- data.table(
    set.name=c("test","train")
  )[
  , data.table(row_id=reg.bench.row[[set.name]][[1]])
  , by=set.name]
  i.points <- set.ids[
    task.dt, on="row_id"
  ][
    is.na(set.name), set.name := "unused"
  ]
  point.dt.list[[score.i]] <- data.table(
    reg.bench.row[, .(task_id, iteration)],
    i.points)
  i.learner <- reg.bench.row$learner[[1]]
  pred.dt.list[[score.i]] <- data.table(
    reg.bench.row[, .(
      task_id, iteration, algorithm
    )],
    as.data.table(
      i.learner$predict(grid.task)
    )[, .(x=grid.dt$x, y=response)]
  )
}
(pred.dt <- rbindlist(pred.dt.list))
(point.dt <- rbindlist(point.dt.list))
set.colors <- c(
  train="#1B9E77",
  test="#D95F02",
  unused="white")
algo.colors <- c(
  featureless="blue",
  rpart="red")
make_person_group <- function(DT){
  DT[, "person/group" := person]
}
make_person_group(point.dt)
make_person_group(reg.bench.score)

if(require(animint2)){
  viz <- animint(
    title="Train/predict on subsets, regression",
    pred=ggplot()+
      ggtitle("Predictions for selected train/test split")+
      theme_animint(height=400)+
      scale_fill_manual(values=set.colors)+
      geom_point(aes(
        x, y, fill=set.name),
        showSelected="iteration",
        size=3,
        shape=21,
        data=point.dt)+
      scale_color_manual(values=algo.colors)+
      geom_line(aes(
        x, y, color=algorithm, group=paste(algorithm, iteration)),
        showSelected="iteration",
        data=pred.dt)+
      facet_grid(
        task_id ~ `person/group`,
        labeller=label_both,
        space="free",
        scales="free")+
      scale_y_continuous(
        breaks=seq(-100, 100, by=2)),
    err=ggplot()+
      ggtitle("Test error for each split")+
      theme_animint(height=400)+
      scale_y_log10(
        "Mean squared error on test set")+
      scale_fill_manual(values=algo.colors)+
      scale_x_discrete(
        "People/groups in train set")+
      geom_point(aes(
        train.groups, regr.mse, fill=algorithm),
        shape=1,
        size=5,
        stroke=2,
        color="black",
        color_off=NA,
        clickSelects="iteration",
        data=reg.bench.score)+
      facet_grid(
        task_id ~ `person/group`,
        labeller=label_both,
        scales="free"),
    diagram=ggplot()+
      ggtitle("Select train/test split")+
      theme_bw()+
      theme_animint(height=300)+
      facet_grid(
        . ~ train.groups,
        scales="free",
        space="free")+
      scale_size_manual(values=c(group=3, fold=1))+
      scale_color_manual(values=c(group="orange", fold="grey50"))+
      geom_rect(aes(
        xmin=-Inf, xmax=Inf,
        color=rows,
        size=rows,
        ymin=display_row, ymax=display_end),
        fill=NA,
        data=inst$viz.rect.dt)+
      scale_fill_manual(values=set.colors)+
      geom_rect(aes(
        xmin=iteration-rect.expand, ymin=display_row,
        xmax=iteration+rect.expand, ymax=display_end,
        fill=set.name),
        clickSelects="iteration",
        data=inst$viz.set.dt)+
      geom_text(aes(
        ifelse(rows=="group", Inf, -Inf),
        (display_row+display_end)/2,
        hjust=ifelse(rows=="group", 1, 0),
        label=paste0(rows, "=", ifelse(rows=="group", group, fold))),
        data=data.table(train.name="same", inst$viz.rect.dt))+
      scale_x_continuous(
        "Split number / cross-validation iteration")+
      scale_y_continuous(
        "Row number"),
    source="https://github.com/tdhock/mlr3resampling/blob/main/vignettes/ResamplingSameOtherCV.Rmd")
  viz
}
if(FALSE){
  animint2pages(viz, "2023-12-13-train-predict-subsets-regression")
}

## -----------------------------------------------------------------------------
N <- 200
library(data.table)
(full.dt <- data.table(
  label=factor(rep(c("spam","not spam"), l=N)),
  person=rep(1:2, each=0.5*N)
)[, signal := ifelse(label=="not spam", 0, 3)][])

## -----------------------------------------------------------------------------
set.seed(1)
n.people <- length(unique(full.dt$person))
for(person.i in 1:n.people){
  use.signal.vec <- list(
    easy=rep(if(person.i==1)TRUE else FALSE, N),
    impossible=full.dt$person==person.i)
  for(task_id in names(use.signal.vec)){
    use.signal <- use.signal.vec[[task_id]]
    full.dt[
    , paste0("x",person.i,"_",task_id) := ifelse(
      use.signal, signal, 0
    )+rnorm(N)][]
  }
}
full.dt

## -----------------------------------------------------------------------------
(scatter.dt <- nc::capture_melt_multiple(
  full.dt,
  column="x[12]",
  "_",
  task_id="easy|impossible"))

## -----------------------------------------------------------------------------
if(require(animint2)){
  ggplot()+
    geom_point(aes(
      x1, x2, color=label),
      shape=1,
      data=scatter.dt)+
    facet_grid(
      task_id ~ person,
      labeller=label_both)
}

## -----------------------------------------------------------------------------
class.task.list <- list()
for(task_id in c("easy","impossible")){
  feature.names <- grep(task_id, names(full.dt), value=TRUE)
  task.col.names <- c(feature.names, "label", "person")
  task.dt <- full.dt[, task.col.names, with=FALSE]
  class.task.list[[task_id]] <- mlr3::TaskClassif$new(
    task_id, task.dt, target="label"
  )$set_col_roles(
    "person",c("group","stratum")
  )$set_col_roles(
    "label",c("target","stratum"))
}
class.task.list

## -----------------------------------------------------------------------------
(class_same_other <- mlr3resampling::ResamplingSameOtherCV$new())

## -----------------------------------------------------------------------------
(class.learner.list <- list(
  if(requireNamespace("rpart"))mlr3::LearnerClassifRpart$new(),
  mlr3::LearnerClassifFeatureless$new()))

## -----------------------------------------------------------------------------
(class.bench.grid <- mlr3::benchmark_grid(
  class.task.list,
  class.learner.list,
  class_same_other))

## -----------------------------------------------------------------------------
if(FALSE){
  if(require(future))plan("multisession")
}
if(require(lgr))get_logger("mlr3")$set_threshold("warn")
(class.bench.result <- mlr3::benchmark(
  class.bench.grid, store_models = TRUE))

## -----------------------------------------------------------------------------
class.bench.score <- mlr3resampling::score(class.bench.result)
class.bench.score[1]

## -----------------------------------------------------------------------------
if(require(animint2)){
  ggplot()+
    geom_point(aes(
      classif.ce, train.groups, color=algorithm),
      shape=1,
      data=class.bench.score)+
    facet_grid(
      person ~ task_id,
      labeller=label_both,
      scales="free")
}

## ----SimulationsAnimintClassification-----------------------------------------
inst <- class.bench.score$resampling[[1]]$instance
rect.expand <- 0.2
grid.value.dt <- scatter.dt[
, lapply(.SD, function(x)do.call(seq, c(as.list(range(x)), l=21)))
, .SDcols=c("x1","x2")]
grid.class.dt <- data.table(
  label=full.dt$label[1],
  do.call(
    CJ, grid.value.dt
  )
)
class.pred.dt.list <- list()
class.point.dt.list <- list()
for(score.i in 1:nrow(class.bench.score)){
  class.bench.row <- class.bench.score[score.i]
  task.dt <- data.table(
    class.bench.row$task[[1]]$data(),
    class.bench.row$resampling[[1]]$instance$id.dt)
  names(task.dt)[2:3] <- c("x1","x2")
  set.ids <- data.table(
    set.name=c("test","train")
  )[
  , data.table(row_id=class.bench.row[[set.name]][[1]])
  , by=set.name]
  i.points <- set.ids[
    task.dt, on="row_id"
  ][
    is.na(set.name), set.name := "unused"
  ][]
  class.point.dt.list[[score.i]] <- data.table(
    class.bench.row[, .(task_id, iteration)],
    i.points)
  if(class.bench.row$algorithm!="featureless"){
    i.learner <- class.bench.row$learner[[1]]
    i.learner$predict_type <- "prob"
    i.task <- class.bench.row$task[[1]]
    setnames(grid.class.dt, names(i.task$data()))
    grid.class.task <- mlr3::TaskClassif$new(
      "grid", grid.class.dt, target="label")
    pred.grid <- as.data.table(
      i.learner$predict(grid.class.task)
    )[, data.table(grid.class.dt, prob.spam)]
    names(pred.grid)[2:3] <- c("x1","x2")
    pred.wide <- dcast(pred.grid, x1 ~ x2, value.var="prob.spam")
    prob.mat <- as.matrix(pred.wide[,-1])
    contour.list <- contourLines(
      grid.value.dt$x1, grid.value.dt$x2, prob.mat, levels=0.5)
    class.pred.dt.list[[score.i]] <- data.table(
      class.bench.row[, .(
        task_id, iteration, algorithm
      )],
      data.table(contour.i=seq_along(contour.list))[, {
        do.call(data.table, contour.list[[contour.i]])[, .(level, x1=x, x2=y)]
      }, by=contour.i]
    )
  }
}
(class.pred.dt <- rbindlist(class.pred.dt.list))
(class.point.dt <- rbindlist(class.point.dt.list))

set.colors <- c(
  train="#1B9E77",
  test="#D95F02",
  unused="white")
algo.colors <- c(
  featureless="blue",
  rpart="red")
make_person_group <- function(DT){
  DT[, "person/group" := person]
}
make_person_group(class.point.dt)
make_person_group(class.bench.score)
if(require(animint2)){
  viz <- animint(
    title="Train/predict on subsets, classification",
    pred=ggplot()+
      ggtitle("Predictions for selected train/test split")+
      theme_animint(height=400)+
      scale_fill_manual(values=set.colors)+
      scale_color_manual(values=c(spam="black","not spam"="white"))+
      geom_point(aes(
        x1, x2, color=label, fill=set.name),
        showSelected="iteration",
        size=3,
        stroke=2,
        shape=21,
        data=class.point.dt)+
      geom_path(aes(
        x1, x2, 
        group=paste(algorithm, iteration, contour.i)),
        showSelected=c("iteration","algorithm"),
        color=algo.colors[["rpart"]],
        data=class.pred.dt)+
      facet_grid(
        task_id ~ `person/group`,
        labeller=label_both,
        space="free",
        scales="free")+
      scale_y_continuous(
        breaks=seq(-100, 100, by=2)),
    err=ggplot()+
      ggtitle("Test error for each split")+
      theme_animint(height=400)+
      theme(panel.margin=grid::unit(1, "lines"))+
      scale_y_continuous(
        "Classification error on test set",
        breaks=seq(0, 1, by=0.25))+
      scale_fill_manual(values=algo.colors)+
      scale_x_discrete(
        "People/groups in train set")+
      geom_hline(aes(
        yintercept=yint),
        data=data.table(yint=0.5),
        color="grey50")+
      geom_point(aes(
        train.groups, classif.ce, fill=algorithm),
        shape=1,
        size=5,
        stroke=2,
        color="black",
        color_off=NA,
        clickSelects="iteration",
        data=class.bench.score)+
      facet_grid(
        task_id ~ `person/group`,
        labeller=label_both),
    diagram=ggplot()+
      ggtitle("Select train/test split")+
      theme_bw()+
      theme_animint(height=300)+
      facet_grid(
        . ~ train.groups,
        scales="free",
        space="free")+
      scale_size_manual(values=c(group=3, fold=1))+
      scale_color_manual(values=c(group="orange", fold="grey50"))+
      geom_rect(aes(
        xmin=-Inf, xmax=Inf,
        color=rows,
        size=rows,
        ymin=display_row, ymax=display_end),
        fill=NA,
        data=inst$viz.rect.dt)+
      scale_fill_manual(values=set.colors)+
      geom_rect(aes(
        xmin=iteration-rect.expand, ymin=display_row,
        xmax=iteration+rect.expand, ymax=display_end,
        fill=set.name),
        clickSelects="iteration",
        data=inst$viz.set.dt)+
      geom_text(aes(
        ifelse(rows=="group", Inf, -Inf),
        (display_row+display_end)/2,
        hjust=ifelse(rows=="group", 1, 0),
        label=paste0(rows, "=", ifelse(rows=="group", group, fold))),
        data=data.table(train.name="same", inst$viz.rect.dt))+
      scale_x_continuous(
        "Split number / cross-validation iteration")+
      scale_y_continuous(
        "Row number"),
    source="https://github.com/tdhock/mlr3resampling/blob/main/vignettes/ResamplingSameOtherCV.Rmd")
  viz
}
if(FALSE){
  animint2pages(viz, "2023-12-13-train-predict-subsets-classification")
}

## -----------------------------------------------------------------------------
sessionInfo()

