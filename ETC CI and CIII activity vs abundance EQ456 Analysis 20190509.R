setwd("E:/Rapa Pers Manuscript/Figure CI and CIII")
data <- read.csv("ETC CI and CIII activity vs abundance EQ456.csv", stringsAsFactors = F)


d.m <- data[data$Sex=="M",]
d.f <- data[data$Sex=="F",]


##m.CIcore <- lm(d.m$norm.CI.act.CI.core ~ d.m$Condition)
##summary(m.CIcore)

# Call:
#   lm(formula = d.m$norm.CI.act.CI.core ~ d.m$Condition)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.43150 -0.10164 -0.00897  0.10391  0.39655 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         1.00000    0.06783  14.744 3.85e-14 ***
#   d.m$ConditionPers  -0.46957    0.09592  -4.895 4.43e-05 ***
#   d.m$ConditionRapa  -0.53818    0.10360  -5.195 2.01e-05 ***
#   d.m$ConditionYoung -0.82473    0.09592  -8.598 4.46e-09 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1918 on 26 degrees of freedom
# Multiple R-squared:  0.7448,	Adjusted R-squared:  0.7154 
# F-statistic:  25.3 on 3 and 26 DF,  p-value: 7.11e-08

##anova(m.CIcore)
# Analysis of Variance Table
# 
# Response: d.m$norm.CI.act.CI.core
# Df  Sum Sq Mean Sq F value   Pr(>F)    
# d.m$Condition  3 2.79288 0.93096  25.297 7.11e-08 ***
#   Residuals     26 0.95685 0.03680                     
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1




m.CIcoreAOV <- aov(d.m$norm.CI.act.CI.core ~ d.m$Condition)
anova(m.CIcoreAOV)
# Analysis of Variance Table
# 
# Response: d.m$norm.CI.act.CI.core
# Df  Sum Sq Mean Sq F value   Pr(>F)    
# d.m$Condition  3 2.79288 0.93096  25.297 7.11e-08 ***
#   Residuals     26 0.95685 0.03680                     
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
TukeyHSD(m.CIcoreAOV)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = d.m$norm.CI.act.CI.core ~ d.m$Condition)
# 
# $`d.m$Condition`
# diff        lwr          upr     p adj
# Pers-Ctl   -0.46956893 -0.7327061 -0.206431745 0.0002453
# Rapa-Ctl   -0.53817992 -0.8224006 -0.253959279 0.0001125
# Young-Ctl  -0.82473303 -1.0878702 -0.561595843 0.0000000
# Rapa-Pers  -0.06861099 -0.3528316  0.215609652 0.9102497
# Young-Pers -0.35516410 -0.6183013 -0.092026912 0.0052412
# Young-Rapa -0.28655310 -0.5707738 -0.002332459 0.0476120

m.CIAOV <- aov(d.m$norm.CI.act.CI.core...acc ~ d.m$Condition)
anova(m.CIAOV)
# Analysis of Variance Table
# 
# Response: d.m$norm.CI.act.CI.core...acc
# Df Sum Sq Mean Sq F value    Pr(>F)    
# d.m$Condition  3 2.6705 0.89017  23.268 1.568e-07 ***
#   Residuals     26 0.9947 0.03826                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
TukeyHSD(m.CIAOV)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = d.m$norm.CI.act.CI.core...acc ~ d.m$Condition)
# 
# $`d.m$Condition`
# diff        lwr         upr     p adj
# Pers-Ctl   -0.45332339 -0.7216147 -0.18503213 0.0004829
# Rapa-Ctl   -0.54351904 -0.8333067 -0.25373135 0.0001279
# Young-Ctl  -0.80339365 -1.0716849 -0.53510239 0.0000001
# Rapa-Pers  -0.09019564 -0.3799833  0.19959205 0.8281865
# Young-Pers -0.35007026 -0.6183615 -0.08177899 0.0071110
# Young-Rapa -0.25987462 -0.5496623  0.02991307 0.0905029


m.CIIIAOV <- aov(d.m$norm.CIII.CIII.abund ~ d.m$Condition)
anova(m.CIIIAOV)
# Analysis of Variance Table
# 
# Response: d.m$norm.CIII.CIII.abund
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# d.m$Condition  3 1.33354 0.44451  18.428 1.287e-06 ***
#   Residuals     26 0.62716 0.02412                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
TukeyHSD(m.CIIIAOV)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = d.m$norm.CIII.CIII.abund ~ d.m$Condition)
# 
# $`d.m$Condition`
# diff        lwr        upr     p adj
# Pers-Ctl   -0.0009013296 -0.2139356  0.2121330 0.9999994
# Rapa-Ctl   -0.1054186810 -0.3355220  0.1246847 0.5975615
# Young-Ctl  -0.4971720818 -0.7102064 -0.2841378 0.0000050
# Rapa-Pers  -0.1045173514 -0.3346207  0.1255860 0.6041408
# Young-Pers -0.4962707521 -0.7093051 -0.2832364 0.0000052
# Young-Rapa -0.3917534007 -0.6218568 -0.1616500 0.0004406




f.CIcoreAOV <- aov(d.f$norm.CI.act.CI.core ~ d.f$Condition)
anova(f.CIcoreAOV)
# Analysis of Variance Table
# 
# Response: d.f$norm.CI.act.CI.core
# Df Sum Sq Mean Sq F value   Pr(>F)   
# d.f$Condition  3 1.0086 0.33619   6.554 0.001286 **
#   Residuals     34 1.7440 0.05130                    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
TukeyHSD(f.CIcoreAOV)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = d.f$norm.CI.act.CI.core ~ d.f$Condition)
# 
# $`d.f$Condition`
# diff        lwr         upr     p adj
# Pers-Ctl   -0.2426864 -0.5045960  0.01922319 0.0775041
# Rapa-Ctl   -0.3543053 -0.6278609 -0.08074962 0.0069349
# Young-Ctl  -0.4644321 -0.7803069 -0.14855721 0.0019091
# Rapa-Pers  -0.1116189 -0.3735285  0.15029073 0.6610883
# Young-Pers -0.2217457 -0.5275902  0.08409885 0.2237619
# Young-Rapa -0.1101268 -0.4260017  0.20574806 0.7827844


f.CIAOV <- aov(d.f$norm.CI.act.CI.core...acc ~ d.f$Condition)
anova(f.CIAOV)
# Analysis of Variance Table
# 
# Response: d.f$norm.CI.act.CI.core...acc
# Df  Sum Sq Mean Sq F value   Pr(>F)   
# d.f$Condition  3 0.95748 0.31916  6.5062 0.001344 **
#   Residuals     34 1.66788 0.04906                    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
TukeyHSD(f.CIAOV)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = d.f$norm.CI.act.CI.core...acc ~ d.f$Condition)
# 
# $`d.f$Condition`
# diff        lwr         upr     p adj
# Pers-Ctl   -0.29120657 -0.5473339 -0.03507928 0.0207615
# Rapa-Ctl   -0.38056638 -0.6480826 -0.11305015 0.0027308
# Young-Ctl  -0.40437071 -0.7132718 -0.09546957 0.0062774
# Rapa-Pers  -0.08935980 -0.3454871  0.16676749 0.7824223
# Young-Pers -0.11316414 -0.4122564  0.18592810 0.7379033
# Young-Rapa -0.02380433 -0.3327055  0.28509681 0.9967489


f.CIIIAOV <- aov(d.f$norm.CIII.CIII.abund ~ d.f$Condition)
anova(f.CIIIAOV)
# Analysis of Variance Table
# 
# Response: d.f$norm.CIII.CIII.abund
# Df Sum Sq Mean Sq F value    Pr(>F)    
# d.f$Condition  3 1.5209 0.50697  13.079 6.244e-06 ***
#   Residuals     36 1.3955 0.03876                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
TukeyHSD(f.CIIIAOV)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = d.f$norm.CIII.CIII.abund ~ d.f$Condition)
# 
# $`d.f$Condition`
# diff        lwr         upr     p adj
# Pers-Ctl   -0.36245027 -0.5894901 -0.13541046 0.0006924
# Rapa-Ctl   -0.44692943 -0.6786127 -0.21524621 0.0000474
# Young-Ctl  -0.52655113 -0.7878616 -0.26524072 0.0000234
# Rapa-Pers  -0.08447916 -0.3058183  0.13685998 0.7343333
# Young-Pers -0.16410087 -0.4162854  0.08808366 0.3124676
# Young-Rapa -0.07962170 -0.3359946  0.17675123 0.8367731


colnames(d.f)


##### Plot the data #####
stripchart(norm.CI.act.CI.core ~ Condition, data=d.f,
           vertical=T, las=1, pch=19, method="jitter", jitter=0.15,
           main="CI core - Females", ylim=c(0,2))

stripchart(norm.CI.act.CI.core...acc ~ Condition, data=d.f,
           vertical=T, las=1, pch=19, method="jitter", jitter=0.15,
           main="CI core + acc - Females", ylim=c(0,2))

stripchart(norm.CIII.CIII.abund ~ Condition, data=d.f,
           vertical=T, las=1, pch=19, method="jitter", jitter=0.15,
           main="CIII - Females", ylim=c(0,2))




stripchart(norm.CI.act.CI.core ~ Condition, data=d.m,
           vertical=T, las=1, pch=19, method="jitter", jitter=0.15,
           main="CI core - Males", ylim=c(0,2))

stripchart(norm.CI.act.CI.core...acc ~ Condition, data=d.m,
           vertical=T, las=1, pch=19, method="jitter", jitter=0.15,
           main="CI core + acc - Males", ylim=c(0,2))

stripchart(norm.CIII.CIII.abund ~ Condition, data=d.m,
           vertical=T, las=1, pch=19, method="jitter", jitter=0.15,
           main="CIII - Males", ylim=c(0,2))
