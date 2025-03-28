import numpy as np
from sklearn.preprocessing import MinMaxScaler


def Scale(X):
    scaler = MinMaxScaler()
    XScale = scaler.fit_transform(X)
    return XScale


def convertArray(X):
    return np.array(X)


def dot_comput(x, y):
    return x.dot(y)


def getRavel(x):
    x = convertArray(x)
    return x.ravel()
