import math
from math import sqrt,log
import numpy as np
from numba import cuda
import numba as nb
import random
from numba.cuda.random import create_xoroshiro128p_states, xoroshiro128p_normal_float64
from scipy.stats import norm, multivariate_normal
import pandas as pd
TPB = 5


@cuda.jit
def power_kernel(R1,R2,power):
    #R1 is the sample points for calculating cdf 
    #R2 is the sample points for calculating power
    thread_x = cuda.grid(1)
    tx= cuda.threadIdx.x
    RR2 = cuda.shared.array(shape=(512,5),dtype = nb.float64)
    xx = cuda.local.array(shape=5,dtype = nb.float64)
   
    n = R1.shape[1]
    for i in range(n):
        xx[i] = R1[thread_x,i]

    p=0
    for ii in range(20):
        #Preload data into shared memory
        for j in range(n):
            RR2[tx,j] = R2[tx+512*ii,j]
        cuda.syncthreads()
        #Count the number of points falling into the domain
        for jj in range(512):
            flag=0
            for kk in range(n):
                if xx[kk]<=RR2[(jj+tx)%512,kk]: #jj
                    flag+=1
            
            if flag==n:
                p+=1
        cuda.syncthreads()
    power[thread_x] = p/5120/2

  
  
def Power_sampling(p_R1,p_R2,p_r,p_It,p_alpha):
    p_alpha = np.array(p_alpha)
    p_N=p_alpha.shape[0]
    p_n=p_alpha.shape[1]
    p_a = np.empty_like(p_alpha)
    for i in range(p_N):
      for j in range(p_n):
        p_a[i,j]=norm.ppf(1-p_alpha[i,j])
    r_It=np.zeros(p_n*p_n).reshape(p_n,p_n)
    for i in range(p_n):
        r_It[i,i]=sqrt(p_r[i]*p_It)
    
    random1 =  p_R1
    random2 = p_R2
    #5 streams in parallel 
    num_streams = 5
    streams=list()
    for j in range(5):
        streams.append(cuda.stream())
  
    power = np.empty(p_N)
    
    R1_0 = cuda.to_device(random1,stream=streams[0])
    R1_1 = cuda.to_device(random1,stream=streams[1])
    R1_2 = cuda.to_device(random1,stream=streams[2])
    R1_3 = cuda.to_device(random1,stream=streams[3])
    R1_4 = cuda.to_device(random1,stream=streams[4])
    
    threads_per_block=512
    blocks = 40
    for i in range(p_N//5):
        
        random2_0 = p_a[i*5  ,]-np.dot(random2,r_It)
        random2_1 = p_a[i*5+1,]-np.dot(random2,r_It)
        random2_2 = p_a[i*5+2,]-np.dot(random2,r_It)
        random2_3 = p_a[i*5+3,]-np.dot(random2,r_It)
        random2_4 = p_a[i*5+4,]-np.dot(random2,r_It)
    
        R2_0 = cuda.to_device(random2_0,stream=streams[0])
        power_0 = cuda.device_array(shape=5120*4,dtype=np.float64,stream=streams[0])
        R2_1 = cuda.to_device(random2_1,stream=streams[1])
        power_kernel[blocks,threads_per_block,streams[0]](R1_0,R2_0,power_0)
    
        power_1 = cuda.device_array(shape=5120*4,dtype=np.float64,stream=streams[1])
        R2_2 = cuda.to_device(random2_2,stream=streams[2])
        power_kernel[blocks,threads_per_block,streams[1]](R1_1,R2_1,power_1)
    
        power_2 = cuda.device_array(shape=5120*4,dtype=np.float64,stream=streams[2])
        R2_3 = cuda.to_device(random2_3,stream=streams[3])
        power_kernel[blocks,threads_per_block,streams[2]](R1_2,R2_2,power_2)
    
        power_3 = cuda.device_array(shape=5120*4,dtype=np.float64,stream=streams[3])
        R2_4 = cuda.to_device(random2_4,stream=streams[4])
        power_kernel[blocks,threads_per_block,streams[3]](R1_3,R2_3,power_3)
    
        power_4 = cuda.device_array(shape=5120*4,dtype=np.float64,stream=streams[4])
        power_kernel[blocks,threads_per_block,streams[4]](R1_4,R2_4,power_4)
    

        p0 = power_0.copy_to_host(stream=streams[0]) 
        pp0 = np.mean(p0)
        p1 = power_1.copy_to_host(stream=streams[1]) 
        pp1 = np.mean(p1)
        p2 = power_2.copy_to_host(stream=streams[2]) 
        pp2 = np.mean(p2)
        p3 = power_3.copy_to_host(stream=streams[3]) 
        pp3 = np.mean(p3)
        p4 = power_4.copy_to_host(stream=streams[4]) 
        pp4 = np.mean(p4)
        power[i*5:(i+1)*5] = np.array([pp0,pp1,pp2,pp3,pp4])
  
    return (1-power)
  
