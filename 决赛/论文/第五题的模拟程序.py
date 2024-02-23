import numpy as np
from math import *
# 下面引入修饰器numba库，可以加速模拟过程
# 如果您的电脑上没有安装numba库，注释掉该行，以及所有的@njit修饰器
from numba import njit  

@njit
def simulate_response(n:int, p_treatment=0.55, p_placebo=0.30) -> (int,bool):
    """n为样本量，按照1:1将被试分配到药物组和安慰剂组，利用模拟的方式，
    计算药物组和安慰剂组一共应答的人数，也即【疗效观测值】"""
    sig = False
    responses_control = np.random.binomial(n//2,p_placebo)
    responses_case = np.random.binomial(n//2,p_treatment)
    xbar1 = responses_control/(n//2)
    xbar2 = responses_case/(n//2)
    sig = (xbar2 - xbar1 - \
           1.96 * sqrt( (xbar1*(1-xbar1)/(n//2) + xbar2*(1-xbar2)/(n//2)) )) > 0 # 是否具有统计学显著性
    return responses_case + responses_control ,sig 

@njit
def consistency_met(n, total_size=200, target_ratio=0.5) -> bool:
    """假设该国的样本量为n，通过模拟的方式，计算该国的疗效观测值和总的疗效观测值
    并在总的样本具有统计学差异的前提下，判断该国的观测值是否是总体观测值的一半以上，返回布尔值
    """
    sig = False
    while not sig: # 只有在总体样本有显著性意义时才进行后续判断
        country_response, s = simulate_response(n)
        global_response, sig = simulate_response(total_size)
    
    # 检查是否满足一致性标准
    return country_response >= global_response * target_ratio

@njit
def find_sample_size(target_prob=0.80, total_size=200, target_ratio=0.5, simulations=1e5) -> int:
    """给定总的样本量total_size，通过模拟的方式，返回满足一致性条件的最小样本量
    """
    n = 2 # 因为要按照1：1进行入组，所以在该国的样本量必为偶数
    while n <= total_size:
        met_criteria = \
            sum([consistency_met(n, total_size, target_ratio) \
                 for _ in range(simulations)])
        prob = met_criteria / simulations
        if prob >= target_prob:
            return n
        n += 2
    return None

required_sample_size = find_sample_size(0.8,200,0.5,1e5)

print(f"需要的样本量为：{required_sample_size}")