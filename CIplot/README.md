<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orgheadline6">1. DESCRIPTION</a>
<ul>
<li><a href="#orgheadline5">1.1. Usage</a>
<ul>
<li><a href="#orgheadline1">1.1.1. 'htest' object</a></li>
<li><a href="#orgheadline2">1.1.2. 'glm' object (logistic regression only!)</a></li>
<li><a href="#orgheadline3">1.1.3. 'posthocTGH' object (Tukey or Games-Howell method)</a></li>
<li><a href="#orgheadline4">1.1.4. Arguments</a></li>
</ul>
</li>
</ul>
</li>
<li><a href="#orgheadline12">2. Examples</a>
<ul>
<li><a href="#orgheadline9">2.1. 'htest' object</a>
<ul>
<li><a href="#orgheadline7">2.1.1. Difference [log = FALSE (default)]</a></li>
<li><a href="#orgheadline8">2.1.2. Ratio (log = TRUE)</a></li>
</ul>
</li>
<li><a href="#orgheadline10">2.2. 'glm' object</a></li>
<li><a href="#orgheadline11">2.3. 'posthocTGH' object</a></li>
</ul>
</li>
</ul>
</div>
</div>

# DESCRIPTION<a id="orgheadline6"></a>

A function to plot confidential interval for
 such as 'htest', 'glm' (logistic regression only!)
 and 'posthocTGH' {userfriendlyscience} objects.

## Usage<a id="orgheadline5"></a>

### 'htest' object<a id="orgheadline1"></a>

    CIplot(x,
           log = FALSE,
           xlim = NULL, xlab = NULL, main = NULL,
           pch = 21, pcol = "black", pcolbg = "white", pcex = 1,
           cilty = 1, cilwd = 1, cicol = "black",
           v = NULL, vlty = 2, vlwd = 1,  vcol = "black",
           ...)

### 'glm' object (logistic regression only!)<a id="orgheadline2"></a>

    CIplot(x,
           conf.level = 0.95,
           xlim = NULL, xlab = "Odds Ratio",
           main = NULL,
           pch = 21, pcol = "black",
           pcolbg = "white", pcex = 1, cilty = 1, cilwd = 1,
           cicol = "black", vlty = 2, vlwd = 1,  vcol = "black",
           las = NULL,
           ...)

### 'posthocTGH' object (Tukey or Games-Howell method)<a id="orgheadline3"></a>

    CIplot(x,
           xlim = NULL, xlab = "Differences in mean", main = NULL,
           pch = 21, pcol = "black", pcolbg = "white", pcex = 1,
           cilty = 1, cilwd = 1, cicol = "black",
           vlty = 2, vlwd = 1,  vcol = "black",
           las = NULL,
           ...)

### Arguments<a id="orgheadline4"></a>

-   x:
    -   'htest' object, typically result of 't.test', 'binom.test',
        or 'prop.test'.
    -   'glm' object (logistic regression only)
    -   'posthocTGH' object, result of 'posthocTGH' {userfriendlyscience}
-   log: (logical) if log is TRUE, the x axis is drawn logarithmically.
    'htest' object only. Default is FALSE.
-   options of plot
    -   xlim: the x limits (x1, x2) of the plot.
    -   xlab: a title for the x axis: see 'title'.
    -   main: a main title for the plot.
    -   &#x2026;: other options for x-axis.
-   options of point
    -   pch: plotting 'character', i.e., symbol to use.
    -   pcol: color code or name.
    -   pcolgb: background (fill) color for the open plot symbols given by
        'pch = 21:25'.
    -   pcex: character (or symbol) expansion.
-   options of confidence interval line
    -   cilty: line types.
    -   cilwd: line width.
    -   cicol: color code or name.
-   options of an additional line
    -   v: the x-value(s) for vertical line. 'htest' object only.
    -   lty: line types.
    -   lwd: line width.
    -   lcol: color code or name.
-   &#x2026;: other options for x-axis.

# Examples<a id="orgheadline12"></a>

## 'htest' object<a id="orgheadline9"></a>

### Difference [log = FALSE (default)]<a id="orgheadline7"></a>

    set.seed(1234)
    x <- rnorm(10, 10, 2); y <- rnorm(10, 8, 2)
    res <- t.test(x, y)
    CIplot(res)

    x <- binom.test(20, 100, 0.3)
    CIplot(x)

    x <- matrix(c(10, 7, 8, 9), 2, 2, byrow = TRUE)
    res <- prop.test(x)
    CIplot(res)
    
    res <- chisq.test(x)
    CIplot(x)                  ## Warning and no plot

    set.seed(1234)
    x <- cor.test(rnorm(10), rnorm(10))
    CIplot(x, xlim = c(-1, 1))

### Ratio (log = TRUE)<a id="orgheadline8"></a>

    x <- var.test(1:10, (1:10)*1.5)
    CIplot(x, log = TRUE)

    x <- matrix(c(10, 7, 8, 9), 2, 2, byrow = TRUE)
    res <- fisher.test(x)
    CIplot(res, log = TRUE)

    library(exact2x2)
    x <- mcnemar.exact(dat)
    CIplot(x, log = TRUE)

## 'glm' object<a id="orgheadline10"></a>

    library(MASS)
    data(birthwt)
    x <- glm(low ~  age + lwt + smoke + ptl + ht + ui, data = birthwt,
             family = binomial)
    CIplot(x, las = 1)
    
    OR1 <- ORci(x)
    CIplot(OR1, las = 1)

## 'posthocTGH' object<a id="orgheadline11"></a>

    library(userfriendlyscience)
    x <- posthocTGH(warpbreaks$breaks, warpbreaks$tension)
    CIplot(x, las = 1)