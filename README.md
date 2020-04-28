---
title: "Capstone"
author: "Natasja Hirabayashi"
date: "4/28/2020"
output: html_document
---

```{r message=FALSE, warning=FALSE}
library(tidyverse)
library(readxl)
library(Hmisc)
library(ez)
library(knitr)
```

## Write-Up

Inflammation pathways are an essential part of and organism's immune response to both damage and pathogens. Both events cause damage-associated molecular patterns (DAMPs) and pathogen-associated patterns (PAMPs) respectively. After DAMPs or PAMPs are activated, there is a large release of ATP which drives systematic inflammation. Mesenchymal stem cells (MSCs) are explored as therapeutic to mitigate an inflammatory response due to ability to modulate endogenous tissue and immune cells (Parekkadan & Milwid, 2013). 

We believe some of this modulation is due to the expression of the surface enzyme CD73, in conjunction with CD39. This pair is able to convert ATP into adenosine which alleviates some of the inflammatory stress that can cause off-target damage throughout the body. In response to an inflammatory event MSCs are able to release paracrine factor and adjust expression of certain cell surface proteins. However, it is not known whether MSC donor sex impacts the change in CD73 expression in response to inflammation. Sex differences have been shown to play a role in the differences in inflammatory processes (Casimir & Duchateau, 2011). We believe some of these differences can be attributed to surface protein expression related to inflammatory processes. We aim to compare the change in CD73 expression between male and female donor MSCs when stimulated with IL-1β, and inflammatory agent. 

If we stimulate male and female MSCs with IL-1β then we will see a difference in percent change in CD73 expression based on sex. The dependent variable CD73 expression, which will be calculated as percent change in CD73 expression for each cell donor. This variable is measured and continuous. As each measurement of CD73 is continuous and the percent change in expression will be calculated from these data. The experimental unit will be each cell donor. The predictor variable is the sex. Sex is a categorical discrete data of two groups: male and female. Levels of CD73 expression pre- and post- stimulation with IL-1β are linked, however the percent change in expression between different sources are not linked. 

$\mu$ denotes the mean percent change of CD73 expression of the sampled groups. 

$H_0 : \mu_{male percent change CD73 expression} = \mu_{female percent change CD73 expression}$

$H_A : \mu_{male percent change CD73 expression} \neq  \mu_{female percent change CD73 expression}$ 


For the statistical test, a two-sided unpaired t-test will be used to compare the group means. Sample size is based on a power of 90% , making β 10%. The standard ⍺ of 5% will be used. Therefore, the null will be rejected for a p-value less than 0.05.  A two-sided test will be used because our hypothesis does not have directionality. We simply predict a difference of means between the two groups. 

To test this hypothesis, we will randomize the selection of MSC donor cell sources from the core. The only selection requirements are that we need an equal number of male (n=10) and female donors (n= 10) to properly power the experiments. This was calculated using G*Power and an effect size of 1.57733. Independent replicates are individual donor cell sources. This is because expression of CD73 will be based on the donor MSC and are not linked. We expect to see a pattern based on sex however, that may not be observed. The cells will be collected, counted, and aliquoted so that there are 250,000 – 300,000 cells in 0.7 mL of 1% BSA-PBS. The cells will then be stained using a CD73 antibody then flow cytometry and analysis will be completed on each cell population using a Cytek Aurora Zen and SpectroFlow software. This will enable collection of baseline expression. Initially, these means can also be compared to see if there is a sex-based difference in baseline expression. Another plate will be used to first treat the cells with IL-1B for 2 hours prior to staining and flow. Then, we can calculate percent change of CD73 expression. 


## Creation of Data
```{r}
set.seed(1234)
#male data creation
ID <-  1:10
Control <- sample(60:70, size=10)
IL1B <- sample(80:90, size=10)
PercentChange <- ((IL1B -  Control) / Control)*100
male <- data.frame(ID, Control , IL1B, PercentChange) %>%
  pivot_longer(cols=c(Control, IL1B),
               names_to="Treatment",
               values_to="CD73")
male

#female data creation
ControlF <- sample(75:85, size=10)
IL1BF <- sample(90:99, size=10)
PercentChangeF <- ((IL1BF -  ControlF) / ControlF)*100
female<- data.frame(ID, ControlF, IL1BF, PercentChangeF) %>%
  pivot_longer(cols=c(ControlF, IL1BF),
               names_to="Treatment",
               values_to="CD73")
female

#Percent change data frame
data <- data.frame(male$PercentChange,female$PercentChangeF) 
summary(data)
view(data)
```

## Plot Data
```{r}
#female change in CD73 expression after treatment
ggplot(female, aes(x=Treatment, y=CD73, group=ID)) + 
  geom_point(size=2,color="#6600cc", alpha= 1) + xlab("Treatment") + 
  geom_line(color="#ff007f") +
  ylab("CD73 Expression") +
  ggtitle("Change in Female CD73 Expression based upon Treatment") +
  theme(plot.title = element_text(hjust=0.5))

#male change in CD73 expression after treatment
ggplot(male, aes(x=Treatment, y=CD73, group=ID)) + 
  geom_point(size=2, alpha= 1) + xlab("Treatment") + 
  geom_line() +
  ylab("CD73 Expression") +
  ggtitle("Change in Male CD73 Expression based upon Treatment") +
  theme(plot.title = element_text(hjust=0.5))

#Comparison of Male and Female CD73 Expression %Change
female.tidy <- data %>% select(female.PercentChangeF)
male.tidy <- data %>% select(male.PercentChange)


data.u <- data.frame(female.tidy, male.tidy) %>%
  pivot_longer(cols=c(male.PercentChange, female.PercentChangeF),
               names_to="Sex",
               values_to="% Change CD73 Expression")

data.y <- data.u%>%
  group_by(Sex) %>%
  summarise(
     mean=mean("% Change CD73 Expression"),
     sd=sd("% Change CD73 Expression"),
           )
  
ggplot(data.u, aes(Sex, "% Change CD73 Expression")) +
  geom_jitter(width=0.15, size=4) + 
  stat_summary(fun.data = mean_sdl, 
               fun.args = list(mult=1), 
               geom="crossbar", 
               width=0.2, 
               color="red"
               ) +
  ggtitle("Percent Change in CD73 expression after stimulation with IL-1B") +
  theme(plot.title = element_text(hjust=0.5))

```

## Unpaired t-test Monte Carlo
```{r}
#t-test function arguments
alt<- "two.sided"
pairing <- FALSE
var <- TRUE
alpha <- 0.05

# the monte carlo function
  z<-t.test(PercentChange,PercentChangeF, 
            alternative = alt, 
            paired = pairing, 
            var.equal = var, 
            conf.level = 1-alpha) #perform the t-test
  
  p<- z$p.value #get the p-value and store it

p


```
