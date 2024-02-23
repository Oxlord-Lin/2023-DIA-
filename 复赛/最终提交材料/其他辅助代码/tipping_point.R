chi.TF = function(a,b,c,d){
  t2 = rbind(c(a,b),c(c,d))
  # print(t2)
  xt = chisq.test(t2);
  # print(xt$p.value)
  xt$p.value <0.05
}

CI.TF = function(a,b,c,d){
  p0 = d/(c+d);
  p1 = b/(a+b);
  s0 = sqrt(p0*(1-p0)/(c+d));
  s1 = sqrt(p1*(1-p1)/(a+b));
  CI.left = p1-p0-1.96*sqrt(s0^2 + s1^2);
  0 < CI.left
}


a = 33; b = 52; c = 55; d = 30;

m0.list = c()
m1.list = c()
for (m0 in 0:15){
  d.new = d+m0
  c.new = c + 15 - m0
  for (m1 in 0:15){
    a.new = a + m1
    b.new = b + 15 - m1
    # print(c(m0,m1))
    if (m1==0){
      state = chi.TF(a.new,b.new,c.new,d.new)
      next
    }
    if (chi.TF(a.new,b.new,c.new,d.new)!=state){
      m0.list = append(m0.list,m0)
      m1.list = append(m1.list,m1)
      state = chi.TF(a.new,b.new,c.new,d.new)}
  }}

plot(m0.list,m1.list,main = '对卡方检验的临界点分析',
     xlab = '安慰剂组退出试验者的应答人数m0',
     ylab = '药物组退出实验者的非应答人数m1',
     xlim = c(0,15),ylim=c(0,15))



m0.list = c()
m1.list = c()
for (m0 in 0:15){
  d.new = d+m0
  c.new = c + 15 - m0
  for (m1 in 0:15){
    a.new = a + m1
    b.new = b + 15 - m1
    # print(c(m0,m1))
    if (m1==0){
      state = CI.TF(a.new,b.new,c.new,d.new)
      next
    }
    if (CI.TF(a.new,b.new,c.new,d.new)!=state){
      m0.list = append(m0.list,m0)
      m1.list = append(m1.list,m1)
      state = CI.TF(a.new,b.new,c.new,d.new)}
  }}

plot(m0.list,m1.list,main = '对95%置信区间是否包含0的临界点分析',
     xlab = '安慰剂组退出试验者的应答人数m0',
     ylab = '药物组退出实验者的非应答人数m1',
     xlim = c(0,15),ylim=c(0,15))
    
    
