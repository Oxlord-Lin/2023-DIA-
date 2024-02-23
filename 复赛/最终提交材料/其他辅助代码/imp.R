dataf1 = data[which(data$week=='week 24'),3:6]
colnames(dataf1) = c('weight','region','treat','y1')
dataf1$y1 = as.logical(dataf1$y1)
dataf1 = as.data.frame(dataf1)
imp_data = mice(dataf1,method='logreg') # 按多元逻辑回归的方式进行插补
fit = with(imp_data,glm(y1~ region+treat,family = binomial(link='logit'))) # 用插补后的数据拟合多元逻辑回归模型
summary(pool(fit)) # 查看回归结果

# 用插补后依据Rubin法则合并后的数据
# 以地区和分组作为自变量，进行多元逻辑回归的结果如下：
# term estimate std.error statistic     df  p.value
# 1         (Intercept)  0.14576    0.3319    0.4392  75.17 0.661766
# 2        regionEurope  0.75691    0.3931    1.9254  76.82 0.057882
# 3 regionNorth America -0.03036    0.4092   -0.0742 101.74 0.940996
# 4        treatPlacebo -1.00828    0.3292   -3.0627  69.30 0.003122
# 可以发现，该结果与用完整数据进行拟合的效果基本一致

m0 = c(0,0,0,0,0)
m1 = m0
v = m0
for(i in 1:5){
  cmplt = complete(imp_data,i)
  p0 = mean(cmplt[which(cmplt$treat=='Placebo'),]$y1)
  p1 = mean(cmplt[which(cmplt$treat!='Placebo'),]$y1)
  m0[i] = p0
  m1[i] = p1
  v[i] = p0*(1-p0)/100 + p1*(1-p1)/100 # 应答率之差的variance
}
P = pool.scalar(m1-m0,v,n=200,k=2) # 合并多个插补数据集的应答率之差以及标准误
dff = P$qbar
# dff = 0.236
S = sqrt(P$ubar)
# S = 0.06861
CI_95 = c(dff-1.96*S,dff+1.96*S)
# CI_95 = (0.1015 0.3705)  95%置信区间不包含0，仍拒绝原假设