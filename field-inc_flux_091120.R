### Script for Anneka's project
## Ludda Ludwig
## 9/11/20

## Load packages

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(dplyr)

#### Part 1) Field flux data analysis and figures

## Uses NEWfieldfluxdata4.csv
## Load data and fix variable types
dat1=read_csv('NEWfieldfluxdata4.csv')
dat1$date=parse_date((dat1$date),format='%m/%d/%y')
# We want 'date' to be seen correctly as a date for plotting figures.
# But for the analysis, we actually want it to be a categorical variable.
# So I'll make a new variable:
dat1$dateF=as.factor(dat1$date)
# Next we want to order the levels of our factors in a convenient way.
# Usually, we prefer the control treatments first. So level 1 of 'burn' should be 'Unburn.
# and level 1 of 'clip' should be 'Uncut'
dat1$burn=parse_factor(dat1$burn,levels=c('Unburn','Burn'))
dat1$clip=parse_factor(dat1$clip,levels=c('Uncut','Cut','CV'))
# if you don't specify levels, they are ordered alphabetically

## You have 6 blocks; 3 each in burned, unburned. Within each block, you have 3 cut treatments.
## I am going to remake your block variable, because we want it to be 6 not 18 levels.
dat1$block_new=str_sub(dat1$block,1,2)
dat1$block_new=parse_factor(dat1$block_new)
# if you don't specify levels, they are ordered alphabetically
# view the dataset:
dat1

## There are two treatments: burn and clip. These will be our 'fixed effects' (i.e. the ones we care about).
## Block is a 'random effect'. We know fluxes from the same block are more likely to be related spatially;
## Some blocks might be wetter than others, warmer than others, different densities of plants, soil etc.
## But if we aren't explicitly modeling those similarities in a block (and many are unknown) we have 
## to account for the non-independence within a block.
## Similarly, date is a 'random effect'. Fluxes that are measured closer together in time are more likely 
## to be related, due to wether, seasonality, etc. We have to account for this non-independence as well.
## It could turn out that both block and date are not affecting the fluxes. It depends on the scale:
## If the spatial heterogeneity is much larger or smaller than the block placements, than accounting for 
## block won't change change anything. Similarly, if the scale of temporal changes in in minutes or weeks,
## instead of days, we might not see an effect of time. 
## So we will test our 'random effects', and if necessary, include them in the model.
## The appropriate type of model is called 'mixed-effects model' (mixed=random+fixed effects).


## Methane first:
## in the function 'lmer' we have several arguments to specify:
## 'data' is our dataset, dat1
## then we have the model formula with fixed effects: ch4~burn+clip
## this means ch4 is a function of burn and clip treatments, which are additive (not interactive)
## then we have the random effects (1|block/date). The '1' specifies a random mean. If we had continuous
## instead of categorical explanatory variables, we could have specified a random slope & intercept.
## So this is saying fit a random mean by block, and fit a random mean by date.
mod1=lmer(data=dat1,ch4~burn+clip+(1|block_new/dateF))
summary(mod1)
    # Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
    # Formula: ch4 ~ burn + clip + (1 | block_new/dateF)
    # Data: dat1
    # 
    # REML criterion at convergence: -85.2
    # 
    # Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
    # -1.6464 -0.6069 -0.1460  0.3028  3.6194 
    # 
    # Random effects:
    # Groups          Name        Variance Std.Dev.
    # dateF:block_new (Intercept) 0.000000 0.00000 
    # block_new       (Intercept) 0.004055 0.06368 
    # Residual                    0.014862 0.12191 
    # Number of obs: 80, groups:  dateF:block_new, 27; block_new, 6
    # 
    # Fixed effects:
    # Estimate Std. Error       df t value Pr(>|t|)    
    # (Intercept)  0.30927    0.04528  5.76581   6.830 0.000573 ***
    # burnBurn    -0.01693    0.05878  4.10138  -0.288 0.787251    
    # clipCut     -0.02407    0.03318 72.09563  -0.726 0.470458    
    # clipCV      -0.08159    0.03351 72.12278  -2.435 0.017382 *  
    # ---
    # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    # 
    # Correlation of Fixed Effects:
    # (Intr) brnBrn clipCt
    # burnBurn -0.633              
    # clipCut  -0.366  0.000       
    # clipCV   -0.359 -0.007  0.495

## There are two sections in the output we will pay attentions to: 'Random effects' and 'Fixed effects'.
## Start by looking at 'Random effects': We want to know how much our random effects (i.e. date, block)
## contribute to the overall variance in the data, and how much is left over that goes in the rest of the model.
## If the random effects, aren't doing much, we can drop them.
    # Random effects:
    #   Groups          Name        Variance Std.Dev.
    # dateF:block_new (Intercept) 0.000000 0.00000 
    # block_new       (Intercept) 0.004055 0.06368 
    # Residual                    0.014862 0.12191 
# Number of obs: 80, groups:  dateF:block_new, 27; block_new, 6

## We can see that our fixed effects ('Residual') contribute most of the variance. That's good.
## We can see that 'date' contributes basically 0.
## Conclusion: we can drop date as a random effect. This means 5 fewer parameters to fit (5 dates).

## Now let's explore the fixed effets. We can have 'clip' and 'burn' in the model as either additive or interactive.
## Additive means burn treatment has an effect, clip treatment has an effect, 
## and if the measurement is both you just add the effects together.
## Interactive means the effect of clipping is different in the burn than the unburn for example.
## Additive is designated by '+' and interactive by '*'.
## We will try the interactive first, and if it isn't significant than we will use the additive.
mod2=lmer(data=dat1,ch4~burn*clip+(1|block_new))
summary(mod2)
## Let's look at the fixed effects output only:
    # Fixed effects:
    #                   Estimate Std. Error       df t value Pr(>|t|)    
    # (Intercept)       0.29173    0.04840  7.42686   6.027  0.00042 ***
    # burnBurn          0.02252    0.07022  8.19710   0.321  0.75648    
    # clipCut           0.02113    0.04429 70.09455   0.477  0.63477    
    # clipCV           -0.07496    0.04511 70.14266  -1.662  0.10102    
    # burnBurn:clipCut -0.10172    0.06644 70.09455  -1.531  0.13029    
    # burnBurn:clipCV  -0.01587    0.06699 70.11639  -0.237  0.81343      

# The bottom two rows show the interaction terms. Note that they are not significant (p-value >0.05)
# But the burn:Cut interaction is almost significant. 

# We can fit an additive instead.
mod3=lmer(data=dat1,ch4~burn+clip+(1|block_new))
summary(mod3)
## Let's look at the fixed effects output only:
    # Fixed effects:
    #              Estimate Std. Error       df t value Pr(>|t|)    
    # (Intercept)  0.30927    0.04528  5.76581   6.830 0.000573 ***
    # burnBurn    -0.01693    0.05878  4.10138  -0.288 0.787251    
    # clipCut     -0.02407    0.03318 72.09563  -0.726 0.470458    
    # clipCV      -0.08159    0.03351 72.12278  -2.435 0.017382 * 

# These results show that the mean flux for Uncut, Unburn is 0.31. Your first level is always the 'Intercept'.
# The p-value is always a comparison to 0. So the sig for Intercept means Uncut Unburn is not 0, no surprise.
# The subsequent rows, are all adjustments to the Intercept. 
# So the Burn fluxes are 0.0169 less than Unburn/Uncut, The Cut fluxes are 0.024 less than Unburn/Uncut, etc.
# However, only the CV effect is significant.

# Pairwise comparisons are done post-hoc with a Tukey's HSD test.
emmeans(mod3,list(pairwise~clip+burn),adjust='tukey')

## The final step in the data analysis is to test the assumptions of the model and make sure nothing is violated.
## We have already discussed independence and are all set there.
## The two other assumptions are normality and heteroskedasticity (equal variance).
## These are both tested on the model residuals.
## Test normality: the plot created in qqnorm should be linear, if the residuals are normal.
qqnorm(residuals(mod3)) ## It does not look linear, so that's a problem.
## Test for equal variance: If residuals increase or decrease systematically with the fitted values, that's bad
plot(residuals(mod3)~fitted(mod3)) ## Seems to increase with fitted values, so that's a problem.
## The solution to both of these, is to try a transformation. This is actually really common, especially with
# certain types of data. CH4 fluxes almost always require a log transformation to be normal.
## Make a new transformed variable:
dat1$ch4.l=log(dat1$ch4)
## Repeat the model with the transformed variable
mod4=lmer(data=dat1,ch4.l~burn+clip+(1|block_new))
summary(mod4) # note that statistically the results are basically the same
## Retest assumptions
qqnorm(residuals(mod4)) # much better!
plot(residuals(mod4)~fitted(mod4)) # much better!


## Let's make a figure and see if the model results match our expectations.

# I'm going to make some settings that are prettier than the defaults in ggplot:
barchart_settings= theme(plot.title=element_text(size=15,vjust=2))+theme(axis.title.x=element_text(vjust=1,size=17),axis.title.y=element_text(size=17))+
  theme(plot.margin=unit(c(0.3,0.3,0.3,0.3),"cm"))+theme(legend.title=element_blank())+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(colour="black"))+
  theme(axis.text.x=element_text(color="black",angle=30,vjust=0.65,size=15),axis.text.y=element_text(color="black",size=15))

## First we will make a boxplot:
Figure1=ggplot(aes(x=clip,y=ch4,fill=burn),data=dat1)+geom_boxplot()+
  ylab(expression(paste("Flux (",mu,"mol", ~CH[4], ~"m",""^"-2","s",""^"-1",")")))+xlab("")+
  scale_fill_viridis_d(option="A",begin=0.25,end=0.75)+
  barchart_settings;Figure1
## This shows the median, 25-75 percentile in the box, whiskers are roughly 95%

## Next we will make a barchart of means and standard errors. 
## First, make a new dataset with the means and standard errors for plotting.
datmeans=ddply(dat1,c("burn",'clip'),summarise,ch4mean=mean(ch4,na.rm=TRUE),se=sd(ch4,na.rm=TRUE)/sqrt(length(na.omit(ch4)))) 
datmeans=transform(datmeans,lower=ch4mean-se,upper=ch4mean+se)

Figure2=ggplot(aes(x=clip,y=ch4mean,fill=burn),data=datmeans)+geom_col(position='dodge')+
  geom_errorbar(aes(ymax=upper,ymin=lower),data=datmeans,width=0.25,position=position_dodge(width=0.9))+
  ylab(expression(paste("Flux (",mu,"mol", ~CH[4], ~"m",""^"-2","s",""^"-1",")")))+xlab("")+
  scale_fill_viridis_d(option="A",begin=0.25,end=0.75)+
  barchart_settings;Figure2

## The results from our mixed effects model line up well with what we would expect given the figures.

## Now we repeat these steps for the carbon dioxide flux.
## You should find that unlike CH4, 'date' contributes a sizeable random effect. 
## Why does this make sense for CO2 fluxes?
## You should find that burn and clip are interactive.
## Here's my model before testing assumptions:
mod5=lmer(data=dat1,co2~burn*clip+(1|block_new/dateF))
summary(mod5)
## Test assumptions:
qqnorm(residuals(mod5)) # good
plot(residuals(mod5)~fitted(mod5)) # good
## Unlike methane, we do not need to transform co2 fluxes. 
## Methane typically has high variability in the extreme fluxes, leading to non-normal behavior.

## Note that Cut and CV have a significant effect on CO2 fluxes. 
## And under the CV treatment, Burn has a sig effect.
##Let's do pairwise comparisons.
emmeans(mod5,list(pairwise~clip+burn),adjust='tukey')
# Note that there are significant burn effects, but only when the clip treatments are different.

## Now for plotting:
Figure3=ggplot(aes(x=clip,y=co2,fill=burn),data=dat1)+geom_boxplot()+
  ylab(expression(paste("Flux (",mu,"mol", ~CO[2], ~"m",""^"-2","s",""^"-1",")")))+xlab("")+
  scale_fill_viridis_d(option="A",begin=0.25,end=0.75)+
  barchart_settings;Figure3

datmeans2=ddply(dat1,c("burn",'clip'),summarise,co2mean=mean(co2,na.rm=TRUE),se=sd(co2,na.rm=TRUE)/sqrt(length(na.omit(co2)))) 
datmeans2=transform(datmeans2,lower=co2mean-se,upper=co2mean+se)
Figure4=ggplot(aes(x=clip,y=co2mean,fill=burn),data=datmeans2)+geom_col(position='dodge')+
  geom_errorbar(aes(ymax=upper,ymin=lower),data=datmeans2,width=0.25,position=position_dodge(width=0.9))+
  ylab(expression(paste("Flux (",mu,"mol", ~CO[2], ~"m",""^"-2","s",""^"-1",")")))+xlab("")+
  scale_fill_viridis_d(option="A",begin=0.25,end=0.75)+
  barchart_settings;Figure4


### Part 2) Incubation analysis and figures

## Uses NEWincubationdata.csv
## Load data and fix variable types
dat2=read_csv('NEWincubationdata.csv')
dat2$date=parse_date((dat2$date),format='%m/%d/%y')
dat2$block=parse_factor(dat2$block)
dat2$burn=parse_factor(dat2$burn,levels=c('Unburn','Burn'))
dat2$amendment=parse_factor((dat2$amendment),levels=c('control','carbon'))
dat2

# Here we have two fixed effects: 'burn' and 'amendment'.
# We are only analyzing one date, so we won't include that variable.
# Block in this analysis bascially just means the amendments are paired:
# The same soil sample used in block 'a' got two different amendment treatments, and fluxes could be different
# then another soil used in block 'b' even for the same treatment.
# So block is our random effect.

# Start with CH4:
mod6=lmer(data=dat2,ch4~burn*amendment+(1|block))
summary(mod6)
## Checking the random effect first. Block has much less contribution to variance, which is nice.
## The interactions are significant.
## Test assumptions:
qqnorm(residuals(mod6)) 
plot(residuals(mod6)~fitted(mod6))
# with small datasets, it is hard to test these assumptions. These look like they probably need to be log transformed.
dat2$ch4.l=log(dat2$ch4)
mod7=lmer(data=dat2,ch4.l~burn*amendment+(1|block))
summary(mod7)
qqnorm(residuals(mod7)) #better
plot(residuals(mod7)~fitted(mod7)) #better

##Let's do pairwise comparisons.
emmeans(mod7,list(pairwise~amendment+burn),adjust='tukey')

Figure5=ggplot(aes(x=amendment,y=ch4,fill=burn),data=dat2)+geom_boxplot()+
  ylab(expression(paste("Flux (",mu,"mol", ~CH[4], ~"g-soil",""^"-1","d",""^"-1",")")))+xlab("")+
  scale_fill_viridis_d(option="A",begin=0.25,end=0.75)+
  barchart_settings;Figure5

datmeans3=ddply(dat2,c("burn",'amendment'),summarise,ch4mean=mean(ch4,na.rm=TRUE),se=sd(ch4,na.rm=TRUE)/sqrt(length(na.omit(ch4)))) 
datmeans3=transform(datmeans3,lower=ch4mean-se,upper=ch4mean+se)
Figure6=ggplot(aes(x=amendment,y=ch4mean,fill=burn),data=datmeans3)+geom_col(position='dodge')+
  geom_errorbar(aes(ymax=upper,ymin=lower),data=datmeans3,width=0.25,position=position_dodge(width=0.9))+
  ylab(expression(paste("Flux (",mu,"mol", ~CH[4], ~"soil-g",""^"-1","d",""^"-1",")")))+xlab("")+
  scale_fill_viridis_d(option="A",begin=0.25,end=0.75)+
  barchart_settings;Figure6


# Incubation CO2:
mod8=lmer(data=dat2,co2~burn*amendment+(1|block))
summary(mod8)
## Checking the random effect first. Block has no effect.
## So now we need to redo the model with no random effects, which is just an ANOVA
mod9=aov(data=dat2,co2~burn*amendment)
summary(mod9)
## ANOVA output is different:
    #                 Df Sum Sq Mean Sq F value Pr(>F)
    # burn            1  1.197  1.1970   1.563  0.247
    # amendment       1  0.752  0.7523   0.982  0.351
    # burn:amendment  1  1.050  1.0504   1.371  0.275
    # Residuals       8  6.127  0.7659  
# here the 'Residuals' means unexplained variance.
# The interaction is not significant, so let's redo it as additive:
mod10=aov(data=dat2,co2~burn+amendment)
summary(mod10)
# Note there is no overall significance. We could check pairwise differences, but they won't be significant either.


## Test assumptions:
qqnorm(residuals(mod10)) 
plot(residuals(mod10)~fitted(mod10))
# with small datasets, it is hard to test these assumptions. These look like they probably need to be log transformed.
dat2$co2.l=log(dat2$co2)
mod11=aov(data=dat2,co2.l~burn+amendment)
summary(mod11)
qqnorm(residuals(mod11)) #better
plot(residuals(mod11)~fitted(mod11)) #better

Figure7=ggplot(aes(x=amendment,y=co2,fill=burn),data=dat2)+geom_boxplot()+
  ylab(expression(paste("Flux (",mu,"mol", ~CO[2], ~"g-soil",""^"-1","d",""^"-1",")")))+xlab("")+
  scale_fill_viridis_d(option="A",begin=0.25,end=0.75)+
  barchart_settings;Figure7

datmeans4=ddply(dat2,c("burn",'amendment'),summarise,co2mean=mean(co2,na.rm=TRUE),se=sd(co2,na.rm=TRUE)/sqrt(length(na.omit(co2)))) 
datmeans4=transform(datmeans4,lower=co2mean-se,upper=co2mean+se)
Figure8=ggplot(aes(x=amendment,y=co2mean,fill=burn),data=datmeans4)+geom_col(position='dodge')+
  geom_errorbar(aes(ymax=upper,ymin=lower),data=datmeans4,width=0.25,position=position_dodge(width=0.9))+
  ylab(expression(paste("Flux (",mu,"mol", ~CO[2], ~"soil-g",""^"-1","d",""^"-1",")")))+xlab("")+
  scale_fill_viridis_d(option="A",begin=0.25,end=0.75)+
  barchart_settings;Figure8



