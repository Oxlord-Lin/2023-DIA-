"""附件二：样本量重估的编程实现"""

from math import *
from scipy.stats import norm
# ppf_list = norm.ppf(q=[0.1, 0.2, 0.8, 0.9], loc=0, scale=1)

def alpha_func(alpha,tau):
    """根据OBF函数计算校正后的期中分析alpha值"""
    return 1 - norm.cdf(norm.ppf(1-alpha)/sqrt(tau)) 


def CP(alpha,z,tau):
    """计算条件把握度"""
    return 1-norm.cdf( ( norm.ppf(1-alpha) - (z*sqrt(tau) + (1-tau)*z/sqrt(tau))  ) / sqrt(1-tau))



def SSR(n,N_max,eta0,eta1,alpha=0.025,power=0.95,tol=0.01):
    """计算重估后的样本量"""
    z = (eta1-eta0)/sqrt(eta1*(1-eta1)/(n/2) + eta0*(1-eta0)/(n/2))
    N_min = n

    # 计算最大样本量的把握度
    tau = n/N_max
    alpha_new = alpha_func(alpha,tau)
    if CP(alpha_new,z,tau) < power:
        return -1 # 重估样本量超过最大允许样本量
    
    while N_min < N_max:
        N_new = (N_max+N_min)//2
        tau = n/N_new
        alpha_new = alpha_func(alpha,tau)
        cp_new = CP(alpha_new,z,tau)
        if abs(cp_new - power) <= tol:
            return N_new
        if cp_new > power:
            N_max = N_new
        else: # if cp_new < power
            N_min = N_new + 1

    return -2 # tol 太小


def main():
    n = int(input('请输入目前已完成的样本量：'))
    N_max = int(input('请输入试验所允许的最大样本量N_max：'))
    eta0 = float(input('请输入安慰剂组的应答率：'))
    eta1 = float(input('请输入药物组的应答率：'))
    alpha = float(input('请输入未校正的单侧一类错误概概率alpha：'))
    power = float(input('请输入所希望达到的把握度power：'))
    while True:
        tol = float(input('请输入条件把握度与期望把握度的最大容忍误差，将作为二分法迭代停止准则：'))
        N_new= SSR(n,N_max,eta0,eta1,alpha,power,tol)
        if N_new == -1:
            print('重估样本量大于试验所允许的最大样本量，把握度不够，建议终止试验')
            break
        elif N_new == -2:
            print('设置的停止准则tol太小，请重新设置tol')
        else:
            print('重估样本量为',N_new)
            break

if __name__ == '__main__':
    main()