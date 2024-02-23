library(gee)
myData = data[,1:6]
myData$week = factor(myData$week,levels=c('week 4','week 8','week 12','week 16','week 20','week 24'))
colnames(myData) = c('id','week','weight','region','treat','y1')
fit <- gee(y1~week+weight+region+treat+treat*week,id=id,data=myData,corstr = 'unstructured',family = 'binomial')
summary(fit)

# 利用广义估计方程拟合模型的主要结果如下
# Model:
#   Link:                      Logit 
# Variance to Mean Relation: Binomial 
# Correlation Structure:     Unstructured 
# 
# Call:
#   gee(formula = y1 ~ week + weight + region + treat + treat * week, 
#       id = id, data = myData, family = "binomial", corstr = "unstructured")
# 
# Summary of Residuals:
#   Min         1Q     Median         3Q        Max 
# -0.7104984 -0.3027152 -0.1552428  0.4124540  0.9236200 
# 
# Coefficients:
#                           Estimate    Naive S.E.  Naive z   Robust S.E.   Robust z
# (Intercept)              -1.86199429  0.3496230 -5.3257196   0.3648092 -5.1040228
# weekweek 8                0.88836188  0.2311107  3.8438806   0.2457702  3.6146041
# weekweek 12               1.46818995  0.2823656  5.1996056   0.2771771  5.2969390
# weekweek 16               1.72488443  0.2980612  5.7870136   0.2880531  5.9880775
# weekweek 20               1.91942309  0.3198643  6.0007423   0.3098292  6.1951000
# weekweek 24               2.01216619  0.3376028  5.9601580   0.3260655  6.1710493
# weightWeight group 2     -0.23614495  0.2506380 -0.9421755   0.2506232 -0.9422309
# regionEurope              0.74763409  0.2993695  2.4973620   0.2964011  2.5223732
# regionNorth America       0.05580632  0.3234823  0.1725174   0.3321409  0.1680200
# treatPlacebo             -0.39443994  0.4168942 -0.9461392   0.4149862 -0.9504893
# weekweek 8:treatPlacebo  -0.59953101  0.3598567 -1.6660273   0.3628223 -1.6524093
# weekweek 12:treatPlacebo -0.72547594  0.4327674 -1.6763646   0.4345050 -1.6696608
# weekweek 16:treatPlacebo -0.78319055  0.4526123 -1.7303785   0.4454338 -1.7582647
# weekweek 20:treatPlacebo -1.00887956  0.4854820 -2.0780988   0.4725623 -2.1349135
# weekweek 24:treatPlacebo -0.57184144  0.5011783 -1.1409940   0.4927536 -1.1605017
# 
# Estimated Scale Parameter:  1.032417