library(readxl)
# myPath = '***'
# data <- read_exel("myPath")
data <- read_excel("D:/大三上学习资料/DIA数据科学大赛备赛资料/决赛/题目材料/DIA 数据竞赛 决赛 模拟数据表.xlsx")
# 对各列重命名，否则太冗长
colnames(data) = c('id','week','weight','region','treat','y1','y2','y3','disFlg','reason','age','priorUse')
data$week = factor(data$week,
                   levels = c('week 4','week 8','week 12','week 16','week 20','week 24'))

data[which(data$weight=='Weight group 1'),]$weight = 'thin'
data[which(data$weight=='Weight group 2'),]$weight = 'fat'
data$weight = factor(data$weight,levels=c('thin','fat'))
data$region = factor(data$region)
data$treat = factor(data$treat,levels=c('Placebo','Experimental Drug'))
data$y1 = as.logical(data$y1)
data$y2 = as.logical(data$y2)
data$y3 = as.logical(data$y3)
data$reason = factor(data$reason)
data$priorUse = factor(data$priorUse,levels=c('No','Yes'))

# 提取出所有地24周有效的数据
data24 = data[which(data$week=='week 24' & !is.na(data$y1)),]
# 建立对数回归模型
formula = 'y1~treat+weight+region+age'
model = glm(formula,family=poisson(link='log') ,data=data24)
summary(model)
# Call:
#   glm(formula = formula, family = poisson(link = "log"), data = data24)
# 
# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)   
# (Intercept)            -1.410182   0.472665  -2.983  0.00285 **
# treatExperimental Drug  0.518174   0.230265   2.250  0.02443 * 
# weightfat              -0.051770   0.229661  -0.225  0.82165   
# regionEurope            0.386521   0.277366   1.394  0.16346   
# regionNorth America     0.008856   0.310710   0.029  0.97726   
# age                     0.005847   0.008796   0.665  0.50623   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 119.57  on 169  degrees of freedom
# Residual deviance: 110.31  on 164  degrees of freedom
# AIC: 286.31
# 
# Number of Fisher Scoring iterations: 5



# 提取出所有地24周有效的数据
data24 = data[which(data$week=='week 24' & !is.na(data$y1)),]
# 按照体重提取两组数据
data24.fat = data24[which(data24$weight=='fat'),]
data24.thin = data24[which(data24$weight=='thin'),]
formula2 = 'y1~treat+region+age'
model = glm(formula2,family=poisson(link='log') ,data=data24.fat)
summary(model)
# Call:
#   glm(formula = formula2, family = poisson(link = "log"), data = data24.fat)
# 
# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)   
# (Intercept)            -2.34246    0.87781  -2.669  0.00762 **
# treatExperimental Drug  0.66076    0.37294   1.772  0.07643 . 
# regionEurope            1.01890    0.55614   1.832  0.06694 . 
# regionNorth America     0.58191    0.62694   0.928  0.35332   
# age                     0.01288    0.01498   0.860  0.38979   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 48.241  on 67  degrees of freedom
# Residual deviance: 39.788  on 63  degrees of freedom
# AIC: 113.79
# 
# Number of Fisher Scoring iterations: 5
model = glm(formula2,family=poisson(link='log') ,data=data24.thin)
summary(model)
# Call:
#   glm(formula = formula2, family = poisson(link = "log"), data = data24.thin)
# 
# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)  
# (Intercept)            -1.09842    0.55101  -1.993   0.0462 *
# treatExperimental Drug  0.43531    0.29291   1.486   0.1372  
# regionEurope            0.11887    0.33737   0.352   0.7246  
# regionNorth America    -0.18008    0.36632  -0.492   0.6230  
# age                     0.00363    0.01097   0.331   0.7408  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 71.295  on 101  degrees of freedom
# Residual deviance: 67.988  on  97  degrees of freedom
# AIC: 177.99
# 
# Number of Fisher Scoring iterations: 5