import os
import numpy as np
from scipy import sparse
import ray
from sklearn.decomposition import TruncatedSVD
from scipy.io import mmwrite
#import argparse


@ray.remote(num_cpus = 1)
def pcCoefficients(X, K, nComp):
    y = X[:, K] 
    Xi = np.delete(X, K, 1)
    #truncated svd
    svd = TruncatedSVD(n_components=nComp)
    svd.fit(Xi)  
    V=svd.components_.T
    s = svd.singular_values_
    score = Xi@V
    beta = V @ np.diag(1/(s**2)[:nComp]) @ (score.T @ y)
    return list(beta)

def pcNet(data, # X: cell * gene
    nComp= 5): 

    data = data.toarray() if sparse.issparse(data) else data 
    if nComp < 2 or nComp >= data.shape[1]:
        raise ValueError('nComp should be greater or equal than 2 and lower than the total number of genes') 
    else:
        np.random.seed(0)
        n = data.shape[1] # genes  
        
        X_ray = ray.put(data)
        B = np.array(ray.get([pcCoefficients.remote(X_ray, k, nComp) for k in range(n)]))  
            
        A = np.ones((n, n), dtype=float)
        np.fill_diagonal(A, 0)
        for i in range(n):
            A[i, A[i, :]==1] = B[i, :]
        A = sparse.csr_matrix(A)     
        return A


def make_pcNet(data, 
          nComp = 5,
          device_count = 1):
    if ray.is_initialized():
        ray.shutdown()
    if device_count ==-1:
        device_count = os.cpu_count()
    ray.init(num_cpus = device_count)
    print(f'ray init, using {device_count} CPUs')
    net = pcNet(data, 
                nComp = nComp)    
             
    if ray.is_initialized():
        ray.shutdown()
    return net


def make_GGN(Y,
             dirpath,
             save_file = True,
             nComp = 5,
             device_count =1):
        #data = sparse.csr_matrix(mmread(os.path.join(dirpath, "Y.mtx")))
        if dirpath is not None:
            file_name = os.path.join(dirpath, "test", "GGN.mtx")

        net = make_pcNet(Y, 
                        nComp = nComp, 
                        device_count = device_count)
        if save_file:
            os.makedirs(os.path.join(dirpath,"test"), exist_ok = True)
            mmwrite(file_name, net)
        #print(f'GGN is saved in {file_name}')
                
        return net

    

#if __name__ == "__main__":
#    parser = argparse.ArgumentParser()
#    parser.add_argument("--dirpath", type=str, default="data_example/single/")
#    parser.add_argument("--count_device", type=int, default=1)
#    parser.add_argument("--nComp", type=int, default=5)
#    args = parser.parse_args()
#    GGN_net = make_GGN(dirpath = args.dirpath
#                       nComp = 5,
#                       device_count = args.count_device)
#    print(GGN_net)
