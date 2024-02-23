mydata = data[which(data$week=='week 24'&data$y1!='NA'),]
mydata$`Assigned Treatment` = factor(mydata$`Assigned Treatment`,levels=c('treatPlacebo','	
Experimental Drug'))
myglm0 = glm('y1~weight+region+treat',family=binomial(link = 'logit'),data=mydata)
myglm = step(myglm0,trace=F) #逐步回归
summary(myglm) # 查看回归结果
# Call:
#   glm(formula = y1 ~ region + treat, family = binomial(), data = mydata)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.5903  -1.1305  -0.8182   1.1228   1.5856  
# 
# Coefficients:
#                       Estimate Std. Error z value Pr(>|z|)   
# (Intercept)          0.15428    0.34152   0.452  0.65146   
# regionEurope         0.77835    0.39575   1.967  0.04921 * 
#   regionNorth America -0.03275    0.41373  -0.079  0.93690   
# treatPlacebo        -1.04392    0.32400  -3.222  0.00127 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 235.46  on 169  degrees of freedom
# Residual deviance: 218.07  on 166  degrees of freedom
# AIC: 226.07
# 
# Number of Fisher Scoring iterations: 4