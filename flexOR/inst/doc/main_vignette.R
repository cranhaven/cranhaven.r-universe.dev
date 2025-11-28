## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load_data----------------------------------------------------------------
library(mlbench)
data("PimaIndiansDiabetes2")
str(PimaIndiansDiabetes2)

## ----create_df2---------------------------------------------------------------
library(flexOR)
library(gam)
df2 <- dfgam(response="diabetes", 
            nl.predictors=c("age","mass"), 
            other.predictors=c("pedigree"),
            smoother="s", 
            method="AIC", 
            data = PimaIndiansDiabetes2)
df2$df

m2 <- gam(diabetes ~ s(age, df=3.3) + s(mass, df=4.1) + pedigree,
          data=PimaIndiansDiabetes2, family=binomial)

## ----apply_flexOR-------------------------------------------------------------
or2 <- flexOR(data = PimaIndiansDiabetes2, 
              response = "diabetes", 
              formula = ~s(age, 3.3) + s(mass, 4.1) + pedigree)
plot(
  x = or2,
  predictor = "mass",
  ref.value = 40,
  ref.label = "Ref. value",
  col.area = c("grey75", "grey90"),
  main = "Smooth odds ratio for mass",
  xlab = "Body mass index",
  ylab = "Log Odds Ratio (Ln OR)",
  lty = c(1,2,2,3,3),
  round.x = 1,
  conf.level = c(0.8, 0.95)
)

## ----plotly-------------------------------------------------------------------

library(plotly)

p <- plot(
  x = or2,
  predictor = "mass",
  ref.value = 40,
  ref.label = "Reference Label",
  main = "Smooth odds ratio for mass",
  xlab = "Mass",
  ylab = "Log Odds Ratio (Ln OR)",
  lty = c(1,2,2,3,3),
  #xlim = c(18, 67),
  #ylim = c(-4.5, 4.5),
  round.x = 1,
  conf.level = c(0.8, 0.95)
)

tmat <- p$estimates
xref <- p$xref      
mdata <- or2$dataset
jj <- match(sort(unique(mdata$mass)), mdata$mass)

# Plotly to get shaded (two-levels) confidence bands
fig <- plot_ly(x=mdata$mass[jj],
                y=tmat[jj,5],
                type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                showlegend = FALSE, name = '80%UCI')

fig <- fig %>% add_trace(y = ~tmat[jj,3], type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgba(0,100,80,0.3)', 
                line = list(color = 'transparent'),
                showlegend = FALSE, name = '95%UCI')
fig <- fig %>% add_trace(y = ~tmat[jj,2], type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgba(0,100,80,0.3)', 
                line = list(color = 'transparent'),
                showlegend = FALSE, name = '95%LCI')
fig <- fig %>% add_trace(y = ~tmat[jj,4], type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgba(0,100,80,0.3)', 
                line = list(color = 'transparent'),
                showlegend = FALSE, name = '80%LCI')
fig <- fig %>% add_trace(y = ~tmat[jj,1], type = 'scatter', mode = 'lines',
                line = list(color='rgb(0,100,80)'),
                showlegend = FALSE, name = 'LnOR')
fig <- fig %>% add_annotations( x = xref,
                #y = floor(min(tmat[jj,])),
                #y = min(tmat[jj,]),
                y = floor_to(min(tmat[jj,]), to=0.5),
                xref = "x", yref = "y",
                axref = "x", ayref = "y",
                text = paste("Ref. value =",xref),
                showarrow = T,
                ax = xref,
                ay = max(tmat[jj,])/2)

fig <- fig %>% layout(#title = ""
  plot_bgcolor='rgb(229,229,229)',
  xaxis = list(title = "Body mass index",
                gridcolor = 'rgb(255,255,255)',
                showgrid = TRUE,
                showline = FALSE,
                showticklabels = TRUE,
                tickcolor = 'rgb(127,127,127)',
                ticks = 'outside',
                zeroline = FALSE),
  yaxis = list(title = "Log Odds Ratio (Ln OR)",
                gridcolor = 'rgb(255,255,255)',
                showgrid = TRUE,
                showline = FALSE,
                showticklabels = TRUE,
                tickcolor = 'rgb(127,127,127)',
                ticks = 'outside',
                #range = c(-4.5,4.5),
                zeroline = FALSE))
fig

## ----flexOR1------------------------------------------------------------------
pdval <- c (20, 25, 30, 35, 40, 45, 50, 55, 60, 65)
predict(or2, predictor = "mass", ref.value = 40, conf.level = 0.95,
           prediction.values = pdval, ref.label = "Ref.")

## ----flexOR2------------------------------------------------------------------
m2 <- gam(diabetes ~ s(age, df=3.3) + s(mass, df=4.1) + pedigree,
          data=PimaIndiansDiabetes2, family=binomial)
summary(m2)

## ----flexOR3------------------------------------------------------------------
m2.mgcv <- mgcv::gam(diabetes ~ s(age) + s(mass) + pedigree,
                     data=PimaIndiansDiabetes2, 
                     family=binomial, 
                     method="GCV.Cp")
m2.mgcv

summary(m2.mgcv)$edf  

summary(m2.mgcv)

## ----plot---------------------------------------------------------------------
p2 <- plot(
  x = or2,
  predictor = "age",
  ref.value = 50,
  ref.label = "Reference Label",
  main = "Smooth odds ratio for age",
  xlab = "Age (years)",
  ylab = "Log Odds Ratio (Ln OR)",
  lty = c(1,2,2,3,3),
  round.x = 1,
  conf.level = 0.95
)

tmat <- exp(p2$estimates)
xref <- p2$xref      
mdata <- or2$dataset
jj <- match(sort(unique(mdata$age)), mdata$age)

fig <- plot_ly(x=mdata$age[jj],y=tmat[jj,3],
                type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                showlegend = FALSE, name = '95%UCI')

fig <- fig %>% add_trace(y = ~tmat[jj,1], type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgba(0,100,80,0.3)', 
                line = list(color='rgb(0,100,80)'),
                showlegend = FALSE, name = 'LnOR')
fig <- fig %>% add_trace(y = ~tmat[jj,2], type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgba(0,100,80,0.3)', 
                line = list(color = 'transparent'),
                showlegend = FALSE, name = '95%LCI')

fig <- fig %>% add_annotations( x = xref,
                y = floor_to(min(tmat[jj,]), to=0.5),
                xref = "x", yref = "y",
                axref = "x", ayref = "y",
                text = paste("Ref. value =",xref),
                showarrow = T,
                ax = xref,
                ay = 1.05)

fig <- fig %>% layout(#title = ""
  plot_bgcolor='rgb(229,229,229)',
        xaxis = list(title = " ",
                gridcolor = 'rgb(255,255,255)',
                showgrid = TRUE,
                showline = FALSE,
                showticklabels = TRUE,
                tickcolor = 'rgb(127,127,127)',
                ticks = 'outside',                
                tickvals = c(20,30,40,50,60,70,80),
                zeroline = FALSE),
        yaxis = list(title = "Log Odds Ratio (Ln OR)",
                gridcolor = 'rgb(255,255,255)',
                showgrid = TRUE,
                showline = FALSE,
                showticklabels = TRUE,
                tickcolor = 'rgb(127,127,127)',
                ticks = 'outside',
                #range = c(-0.5,3.5),
                zeroline = FALSE))
fig

