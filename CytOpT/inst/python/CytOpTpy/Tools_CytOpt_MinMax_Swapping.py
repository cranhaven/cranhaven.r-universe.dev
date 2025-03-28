import numpy as np
from scipy.stats import entropy


def cost(X_s,y):
    """
    Squared euclidean distance between y and the I points of X_s.
    """
    diff = X_s-y
    return(np.linalg.norm(diff, axis = 1)**2)


def grad_f(lbd, eps, X_s, X_t, j, u, D):
    """
    Compute the gradient with respect to u of the function f inside the expectation
    """
    arg1 = (u - cost(X_s, X_t[j]))/eps
    cor1 = np.max(arg1)
    vec1 = np.exp(arg1-cor1)
    t1 = - vec1/np.sum(vec1)

    arg2 = -(D.T).dot(u)/lbd
    cor2 = np.max(arg2)
    vec2 = np.exp(arg2-cor2)
    t2 = D.dot(vec2)/np.sum(vec2)

    return(t1+t2)


def gammatrix(X_s, Lab_source):
    """
    Computation of the operator D that maps the class proportions with the weights.
    """
    I = X_s.shape[0]
    if min(Lab_source) == 0:
        K = int(max(Lab_source))
        D = np.zeros((I,K+1))
        for k in range(K+1):
            D[:,k] = 1/np.sum(Lab_source == k) * np.asarray(Lab_source == k, dtype=float)

        h = np.ones(K+1)

    else:
        K = int(max(Lab_source))
        D = np.zeros((I,K))
        for k in range(K):
            D[:,k] = 1/np.sum(Lab_source == k+1) * np.asarray(Lab_source == k+1, dtype=float)

        h = np.ones(K)
    return(D,h)


# cytopt
def cytopt_minmax(X_s, X_t, Lab_source, eps=0.0001, lbd=0.0001, n_iter=10000,
                  step=5, power=0.99, theta_true=None, monitoring=False):
    """
    Robbins-Monro algorithm to compute an approximate of the vector u^* solution of the maximization problem
    At each step, it is possible to evaluate the vector h_hat to study the convergence of this algorithm.
    """
    n_iter = int(n_iter)
    I = X_s.shape[0]
    J = X_t.shape[0]
    U = np.zeros(I)
    D = gammatrix(X_s, Lab_source)[0]

    # Step size policy
    if step == 0:
        gamma = I * eps / 1.9
    else:
        gamma = step

    if power == 0:
        c = 0.51
    else:
        c = power

    sample = np.random.choice(J, n_iter)

    # Storage of the KL divergence between theta_hat and theta_true
    KL_storage = np.zeros(n_iter)

    for it in range(1, n_iter):
        idx = sample[it]
        grd = grad_f(lbd, eps, X_s, X_t, idx, U, D)
        U = U + gamma / (it + 1) ** c * grd

        # Computation of the estimate h_hat
        arg = -(D.T).dot(U) / lbd
        M = np.max(arg)

        theta_hat = np.exp(arg - M)
        theta_hat = theta_hat / theta_hat.sum()

        if monitoring and isinstance(theta_true, list):
            KL_storage[it] = entropy(pk=theta_hat, qk=theta_true)


    return (theta_hat, KL_storage)
