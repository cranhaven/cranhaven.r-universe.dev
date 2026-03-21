## ----fig.show='asis',fig.width=7, fig.height=5,warning=FALSE-------------
coffee <- c(58, 11, 5, 53, 53, 59, 24, 59, 46, 20)
fill_coffee <- c(162, 57, 103, 154, 165, 132, 74, 107, 104,  93)
fill_water <- c(66, 92, 54, 78, 74, 114, 91, 129, 71, 56)
push_B <- c(74, 99, 62, 84, 83, 120, 95, 129, 80, 63)
drink <- c(472, 176, 475, 283, 265, 207, 234, 184, 490, 520)
X <- data.frame(id = seq(1,10), coffee, fill_coffee, fill_water, push_B, drink)
head(X)

## ---- warning=FALSE------------------------------------------------------
library(ViSiElse)

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
visi1 <- visielse(X)

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
visi1 <- visielse(X, pixel = 5)

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
visi1 <- visielse(X, pixel = 80)

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
visi1 <- visielse(X, informer = "mean")

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
visi1 <- visielse(X, informer = NULL)

## ----fig.width=7, fig.height=1.5-----------------------------------------
visi1 <- visielse(X, informer = NULL, doplot = FALSE) # automatically creates a ViSibook
book <- visi1@book # Exctract the ViSibook
plot(book)

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
book

## ----fig.show='asis',fig.width=7, fig.height=1.5, warning=FALSE----------
book[,2] <- c("Taking the coffee",
"Fill the machine with coffee",
"Fill the tank with water",
"Push the Button",
"Drink the coffee")
book
plot(book)

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
visi1 <- visielse(X, book = book, is.ViSibook = TRUE, informer = NULL)

## ----fig.show='asis',fig.width=7, fig.height=1.5, warning=FALSE----------
book[,4]<- c(5,1,2,4,3)
plot(book)

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
visi1 <- visielse(X, book = book, is.ViSibook = TRUE)

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
visi1 <- visielse( X )
book <- ConvertFromViSibook(visi1@book) # Convert book into data.frame
add_delay <- c("delay_coffee_push", "Preparation", "l", "6", "coffee", "push_B") # Create the long action (variable name, label, action type, action order, long action starting point, long action ending point)
book[6, ] <- add_delay # Add the long action at the end of the ViSibook
book

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
visi2 <- visielse(X = X, book = book, informer = NULL)

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
book$GZDeb <- c(NA, 60, NA, NA, NA, NA) # New column in the ViSibook, the 2nd action "fill_coffee" should be done after 60s
book$GZFin <- c(NA, 120, NA,NA, NA, NA) # New column in the ViSibook, the 2nd action "fill_coffee" should be done before 120s
visi2 <- visielse(X = X, book = book, informer = NULL)

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
book$GZDeb <- c(NA, NA, NA, NA, NA, NA) # No more green zone
book$GZFin <- c(NA, NA, NA, NA, NA, NA) # No more green zone
book$BZBeforeDeb <- c(NA, 0, NA, NA, NA, NA) # New column in the ViSibook, the 2nd action "fill_coffee" should not be done between 0 and 30s
book$BZBeforeFin <- c(NA, 30, NA, NA, NA, NA) # New column in the ViSibook, the 2nd action "fill_coffee" should not be done between 0 and 30s
visi2 <- visielse(X = X, book = book, informer = NULL)

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
book$GZDeb <- c(NA, 60, NA, NA, NA, NA) # The green zone is back !
book$GZFin <- c(NA, 120, NA, NA, NA, NA) # The green zone is back !
book$BZBeforeDeb <- c(NA, 0, NA, NA, NA, NA) # 1st black zone before the green zone: starting point
book$BZBeforeFin <- c(NA, 30, NA, NA, NA, NA) # 1st black zone before the green zone: ending point
book$BZAfterDeb <- c(NA, 180, NA, NA, NA, NA) # New column in the ViSibook for the 2nd black zone, "fill_coffee" should not be done after 3min
book$BZAfterFin <- c(NA, Inf, NA, NA, NA, NA) # New column in the ViSibook for the 2nd black zone, "fill_coffee" should not be done after 3min
visi2 <- visielse(X = X, book = book, informer = NULL)

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
visi1 <- visielse(X,  doplot = FALSE)
book <- ConvertFromViSibook(visi1@book) # Convert book into data.frame
add_delay <- c("delay_coffee_push", "Preparation", "l", "6", "coffee", "push_B") # We use again the same long action as before
book[6,] <- add_delay 
book$BZLong <- c(rep(NA, 5), 60) # The long action should be done before 60s
book$BZLtype <- c(rep(NA, 5), "time") # The long action time limit is a deadline
visi1 <- visielse(X, book = book, informer = NULL)

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
book$BZLong <- c(rep(NA, 5), 30) # The long action should last maximum 30s
book$BZLtype <- c(rep(NA, 5), "span") # The long action time limit is a duration not to exceed
visi1 <- visielse(X, book = book, informer = NULL)

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
group <- c("group2", "group1", "group2", "group1", "group1", "group2",
           "group1", "group1", "group1", "group2") # group definition for the 10 subjects
visi1 <- visielse(X, group = group, book = book, informer = NULL, method = "cut")

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
group <- c("group2", "group1", "group2", "group1", "group1", "group2",
           "group1", "group1", "group1", "group2") # group definition for the 10 subjects
visi1 <- visielse(X, group = group, book = book, informer = NULL, method = "join")

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
group <- c("group2", "group1", "group2", "group1", "group1", "group2",
           "group1", "group1", "group1", "group2") # group definition for the 10 subjects
visi1 <- visielse(X, group = group, book = book, informer = NULL, method = "within", grwithin = "group1")

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE, eval = FALSE----
#  help("visielse")

