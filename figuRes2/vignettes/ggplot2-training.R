## -----------------------------------------------------------------------------
require(tidyverse)
require(gridExtra)
require(scales)
require(figuRes2)
require(latex2exp)

## -----------------------------------------------------------------------------
x <- rnorm(100)
hist(x)
boxplot(x)
plot(x)

## -----------------------------------------------------------------------------
my.df <- data.frame(x=x)
ggplot(data=my.df, aes(x=x)) + geom_histogram()
ggplot(data=my.df, aes(x=factor(""), y=x)) + geom_boxplot()
ggplot(data=my.df, aes(x=1:100, y=x))+geom_point()

## -----------------------------------------------------------------------------
# dframe=read.csv("demog.csv")
data(demog.data)

## -----------------------------------------------------------------------------
head(demog.data)

## -----------------------------------------------------------------------------
table(demog.data$REGION)
data.frame(table(demog.data$REGION))

## -----------------------------------------------------------------------------
ggplot(data=demog.data, aes(x=REGION)) + geom_bar()

## -----------------------------------------------------------------------------
ggplot(data=demog.data, aes(x=REGION)) + geom_bar() + coord_flip()

## -----------------------------------------------------------------------------
p <- ggplot(data=demog.data, aes(x=REGION)) + geom_bar() + coord_flip()
# print or simply send p to command line
print(p)

## -----------------------------------------------------------------------------
p + labs(title="Barchart of Regions", y = "Count", x="Region", subtitle="My subtitle", caption="My caption")

## -----------------------------------------------------------------------------
p + scale_y_continuous(breaks=seq(0,6000, 500), limits= c(0, 5000))

# Note that tick mark at 5000 is not printed.
p + scale_y_continuous(breaks=seq(0,4500, 500), limits= c(0, 5000))

## -----------------------------------------------------------------------------
p + labs(title="Barchart of Regions", y = "Count", x="Region", subtitle="My subtitle", caption="My caption")+ scale_y_continuous(breaks=seq(0,4000, 500))

## -----------------------------------------------------------------------------
class(demog.data)
demog.data$REGION
class(demog.data$REGION)
levels(demog.data$REGION) # <-- we need to convert this to a factor

## -----------------------------------------------------------------------------
levels(factor(demog.data$REGION)) 

# Indeed, Alphabetical ordering
all.equal(levels(factor(demog.data$REGION)),
          levels(factor(demog.data$REGION, c("Asia/Pacific",
                                         "Eastern Europe",
                                         "North America",
                                         "South America",  
                                         "Western Europe"))))

## -----------------------------------------------------------------------------
levels(factor(demog.data$REGION, c("Asia/Pacific",
                               "Eastern Europe",
                               "Western Europe",
                               "North America",
                               "South America")))

## -----------------------------------------------------------------------------
# We really want to order these by frequency...
data.frame(original.order = levels(factor(demog.data$REGION)),
           original.frequency = data.frame(table(demog.data$REGION))$Freq,
           map=order(table(demog.data$REGION)),
           new.order=levels(factor(demog.data$REGION))[order(table(demog.data$REGION))])

## -----------------------------------------------------------------------------
demog.data$REGION2 <- factor(demog.data$REGION, c("Asia/Pacific",
                                          "Eastern Europe",
                                          "North America",
                                          "South America",  
                                          "Western Europe")[order(table(demog.data$REGION))])

demog.data$REGION3 <- factor(demog.data$REGION, c("Asia/Pacific",
                                          "Eastern Europe",
                                          "North America",
                                          "South America",  
                                          "Western Europe")[rev(order(table(demog.data$REGION)))])

## -----------------------------------------------------------------------------
p1 <- ggplot(data=demog.data, aes(x=REGION)) + geom_bar() + coord_flip()
p2 <- ggplot(data=demog.data, aes(x=REGION2)) + geom_bar() + coord_flip()
p3 <- ggplot(data=demog.data, aes(x=REGION3)) + geom_bar() + coord_flip()

grid.arrange(p1 + labs(title="Default ordering - alphabetical"), 
             p2 + labs(title="Ordered by frequency"),
             p3 + labs(title="Ordered by Frequency & reversed"), ncol=1)

## -----------------------------------------------------------------------------
# Note the use of dplyr::rename to rename columns
demog.data.table.original <- data.frame(table(demog.data$REGION))
demog.data.table.renamed <- demog.data.table.original %>% rename(REGION = Var1)

demog.data.table.original
demog.data.table.renamed

## -----------------------------------------------------------------------------
ggplot(data=demog.data.table.renamed, aes(x=REGION, y=Freq)) + geom_bar(stat="identity")

## -----------------------------------------------------------------------------
demog.data.table.original$prop <- demog.data.table.original$Freq/sum(demog.data.table.original$Freq)
ggplot(data=demog.data.table.original, aes(x=Var1, y=prop)) + geom_bar(stat="identity")

## -----------------------------------------------------------------------------
demog.data.table <- data.frame(table(demog.data$REGION, demog.data$SEX))
demog.data.table$prop <- demog.data.table$Freq/sum(demog.data.table$Freq)

p1 <- ggplot(data = demog.data.table, aes(x = Var1, y = prop, fill = Var2)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  geom_text(position=position_dodge(width=1), size = 5, 
             aes(y=prop+.02, label=paste(" n = ", Freq, sep = "")))+
  scale_y_continuous(limits = c(0,.3), breaks = seq(0,1,.05), labels = percent_format()) + 
  scale_fill_manual(values = c("blue", "brown")) + 
  labs(y = "Proportion", x = "Region", fill = "Gender", title="Barchart of Region and Gender")+
  theme(legend.position= "bottom")
p1

## -----------------------------------------------------------------------------
curve(expr = dnorm(x), from=-6, to=6, n=1001)
temp <- curve(expr = dnorm(x), from=-6, to=6, n=1001)
class(temp)
str(temp)

## ----eval=F-------------------------------------------------------------------
#  head(data.frame(temp))

## -----------------------------------------------------------------------------
head(figuRes2::gcurve(expr = dnorm(x), from=-6, to=6, n=101, category="Standard Normal"))

## -----------------------------------------------------------------------------
my.df <- rbind(
  gcurve(expr = dnorm(x), from=-6, to=6, n=101, category="N(0, 1)"),
  gcurve(expr = dnorm(x, -1, 1), from=-6, to=6, n=101, category="N(-1, 1)"),
  gcurve(expr = dnorm(x, 1, 1), from=-6, to=6, n=101, category="N(1, 1)"))

ggplot(data=my.df, aes(x=x, y=y, color=category))+geom_line()

## -----------------------------------------------------------------------------
grid.arrange(top='Comparison of labs(y=""), labs(y=NULL), and labs(y=NULL) & scale_y_continuous(breaks=NULL)',
ggplot(data=my.df, aes(x=x, y=y, color=category))+
  geom_line(size=.75)+
  labs(x="x", y="", 
       title="Normal Density Functions", 
       color="Density")+
  theme(legend.position="bottom"),

ggplot(data=my.df, aes(x=x, y=y, color=category))+
  geom_line(size=.75)+
  labs(x="x", y=NULL, 
       title="Normal Density Functions", 
       color="Density")+
  theme(legend.position="bottom"),
ggplot(data=my.df, aes(x=x, y=y, color=category))+
  geom_line(size=.75)+
  labs(x="x", y=NULL, 
       title="Normal Density Functions", 
       color="Density")+
  theme(legend.position="bottom")+
  scale_y_continuous(breaks=NULL)
)

## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------
head(demog.data)
str(demog.data)
sort(names(demog.data))

## -----------------------------------------------------------------------------
ggplot(data=demog.data, aes(x=HEIGHT)) + geom_histogram()
summary(demog.data$HEIGHT)

## ----eval=T-------------------------------------------------------------------
# note na.rm = T is needed
max(demog.data$HEIGHT, na.rm=T) - min(demog.data$HEIGHT, na.rm=T)
median(demog.data$HEIGHT)
mean(demog.data$HEIGHT)
median(demog.data$HEIGHT, na.rm=T)
mean(demog.data$HEIGHT, na.rm=T)

## ----eval=T-------------------------------------------------------------------
grid.arrange(top="Comparison of various binwidth values passed to geom_histogram",
ggplot(data=demog.data, aes(x=HEIGHT)) + geom_histogram(binwidth = .5) + ggtitle("binwidth = .5"),
ggplot(data=demog.data, aes(x=HEIGHT)) + geom_histogram(binwidth = 1)+ ggtitle("binwidth = 1"),
ggplot(data=demog.data, aes(x=HEIGHT)) + geom_histogram(binwidth = 2.5)+ ggtitle("binwidth = 2.5"),
ggplot(data=demog.data, aes(x=HEIGHT)) + geom_histogram(binwidth = 25)+ ggtitle("binwidth = 25"),
ncol=2)

## ----eval=T-------------------------------------------------------------------
grid.arrange(top="Comparison of various binwidth values passed to geom_histogram when color and fill differ",
ggplot(data=demog.data, aes(x=HEIGHT)) + geom_histogram(binwidth = .5, color="red", fill="blue") + ggtitle("binwidth = .5"),
ggplot(data=demog.data, aes(x=HEIGHT)) + geom_histogram(binwidth = 1, color="red", fill="blue")+ ggtitle("binwidth = 1"),
ggplot(data=demog.data, aes(x=HEIGHT)) + geom_histogram(binwidth = 2.5, color="red", fill="blue")+ ggtitle("binwidth = 2.5"),
ggplot(data=demog.data, aes(x=HEIGHT)) + geom_histogram(binwidth = 25, color="red", fill="blue")+ ggtitle("binwidth = 25"),
ncol=2)

## ----eval=T-------------------------------------------------------------------
q <- ggplot(data=demog.data, aes(x=HEIGHT)) +  ggtitle("binwidth = 2.5") 

grid.arrange(top="Comparison of various values passed to transparency argument alpha",
  q + geom_histogram(binwidth = 2.5, color="red", fill="blue", alpha=0) + labs(subtitle = "alpha = 0"),
  q + geom_histogram(binwidth = 2.5, color="red", fill="blue", alpha=.2) + labs(subtitle = "alpha = 0.2"),
  q + geom_histogram(binwidth = 2.5, color="red", fill="blue", alpha=.4) + labs(subtitle = "alpha = 0.4"),
  q + geom_histogram(binwidth = 2.5, color="red", fill="blue", alpha=.6) + labs(subtitle = "alpha = 0.6"),
  q + geom_histogram(binwidth = 2.5, color="red", fill="blue", alpha=.8) + labs(subtitle = "alpha = 0.8"),
  q + geom_histogram(binwidth = 2.5, color="red", fill="blue", alpha=1) + labs(subtitle = "alpha = 1.0")
)

## ----eval=T-------------------------------------------------------------------
 q + geom_histogram(binwidth = 2.5, color="red", fill="blue", alpha=.6) + 
  scale_x_continuous(breaks=seq(0,500,10), limits=c(100,225)) + 
  scale_y_continuous(breaks=seq(0, 3000, 100)) +
  labs(subtitle = "alpha = 0.6", caption="Scaling of axes accomplished with:\nscale_x_continuous(breaks=seq(0,500,10), limits=c(100,225))\nscale_y_continuous(breaks=seq(0, 3000, 250))")

## ----eval=T-------------------------------------------------------------------
demog.data$SEX <- factor(demog.data$SEX, c("F", "M")) # Alphabetical ordering
demog.data$SEX2 <- factor(demog.data$SEX, c("M", "F")) # Preferred order
table(demog.data$SEX)
table(demog.data$SEX2)

grid.arrange(top="Comparison of plots when swapping order of factor levels",
ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX)) + 
  geom_histogram(bin=2.5),
ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX2)) + 
  geom_histogram(bin=2.5))

## ----eval=T-------------------------------------------------------------------
grid.arrange(top="Comparison of plots when swapping order of factor levels - transparency added",
ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX)) + 
  geom_histogram(bin=2.5, alpha=.2),
ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX2)) + 
  geom_histogram(bin=2.5, alpha=.2))


## ----eval=T-------------------------------------------------------------------
grid.arrange(top="Comparison of facet_wrap scales options",
ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX)) + 
  geom_histogram(bin=2.5, alpha=.2) + facet_wrap(~SEX) + ggtitle("facet_wrap default"),
ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX)) + 
  geom_histogram(bin=2.5, alpha=.2) + facet_wrap(~SEX, scales="free_x") + ggtitle('scales="free_x"'),
ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX)) + 
  geom_histogram(bin=2.5, alpha=.2) + facet_wrap(~SEX, scales="free_y") + ggtitle('scales="free_y"'),
ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX)) + 
  geom_histogram(bin=2.5, alpha=.2) + facet_wrap(~SEX, scales="free") + ggtitle('scales="free"'), ncol=2)

grid.arrange(top="Comparison of facet_wrap scales options with single column of facets imposed",
ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX)) + 
  geom_histogram(bin=2.5, alpha=.2) + facet_wrap(~SEX, ncol=1) + ggtitle("facet_wrap default, ncol=1"),
ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX)) + 
  geom_histogram(bin=2.5, alpha=.2) + facet_wrap(~SEX, scales="free_x", ncol=1) + ggtitle('scales="free_x", ncol=1'),
ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX)) + 
  geom_histogram(bin=2.5, alpha=.2) + facet_wrap(~SEX, scales="free_y", ncol=1) + ggtitle('scales="free_y", ncol=1'),
ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX)) + 
  geom_histogram(bin=2.5, alpha=.2) + facet_wrap(~SEX, scales="free", ncol=1) + ggtitle('scales="free", ncol=1'), ncol=2)

## -----------------------------------------------------------------------------
get.whiskers <- function(dframe) {
                bplot <- boxplot(dframe$RESPONSE ~ dframe$CATEGORY, plot = F)
                whiskers <- with(bplot, 
                                 data.frame(CATEGORY = names, 
                                            LOWER = stats[1, ], 
                                            MEDIAN = stats[3, ], 
                                            UPPER = stats[5, ],
                                            N=n))
                return(whiskers)
        }
        
whiskers <- get.whiskers(dframe = demog.data %>% select(SUBJID, TRTGRP, BMI) %>% rename(RESPONSE=BMI, CATEGORY=TRTGRP ))

demog.data %>% select(SUBJID, TRTGRP, BMI) %>% ggplot(aes(x=TRTGRP, y = BMI, shape=TRTGRP, fill=TRTGRP)) + geom_boxplot()

demog.data %>% select(SUBJID, TRTGRP, BMI) %>% ggplot(aes(x=TRTGRP, y = BMI, shape=TRTGRP, fill=TRTGRP)) +
geom_boxplot(outlier.size = 3, outlier.colour = alpha("black", 0.2))+
         stat_summary(fun = mean, geom = "point", 
                             shape = c(21, 22), 
                             size = 3, bg = "white") +
                scale_fill_manual(values = c("red", "blue")) +
                scale_x_discrete(breaks = levels(demog.data$TRTGRP), 
                                 labels = paste(levels(demog.data$TRTGRP),"\n n = ", whiskers$N)) + 
                scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) + 
                geom_text(data = whiskers %>% rename(TRTGRP=CATEGORY) %>% mutate(x=as.numeric(as.factor(TRTGRP)) - .5), aes(x = x, y = MEDIAN, label = round(MEDIAN, 2)), 
                          size = 5) + 
                theme(legend.position= "bottom")+ labs(fill="Treatment Group")+
                labs(y = TeX(paste0("BMI kg/", "$m^{2}$")), 
                     x = "Treatment Group", 
                     fill = "Treatment Group",
                     shape = "Treatment Group",
                     title=TeX(paste0("Boxplot of BMI kg/", "$m^{2}$")),
                     caption="The median value is displayed to the left of each boxplot.")

## ----eval=T-------------------------------------------------------------------
ggplot(data=demog.data, aes(x=factor(""), y=HEIGHT)) + geom_violin() 
ggplot(data=demog.data, aes(x=factor(""), y=HEIGHT)) + geom_violin() + labs(x="All Patients", y="Height")
ggplot(data=demog.data, aes(x=SEX, y=HEIGHT, fill=BMI.GRP)) + geom_violin()

ggplot(data=demog.data, aes(x=SEX, y=HEIGHT, fill=SEX)) + geom_violin() + facet_wrap(~REGION)
ggplot(data=demog.data, aes(x=SEX, y=HEIGHT, fill=SEX)) + geom_violin() + facet_wrap(~REGION, nrow=1)
ggplot(data=demog.data, aes(x=SEX, y=HEIGHT, fill=SEX)) + geom_violin() + facet_wrap(BMI.GRP~REGION)
ggplot(data=demog.data, aes(x=SEX, y=HEIGHT, fill=SEX)) + geom_violin() + facet_grid(REGION~BMI.GRP)


