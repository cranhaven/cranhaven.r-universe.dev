#### Data format

In case you don't have access to the full dicentrics distribution, you can choose to only provide total number of dicentrics. In this case the required data are:

- The doses `D` in Gy.
- `N` is the total number of cells.
- `X` is the number of aberrations.

This can be input manually or using a `.csv` (or `.txt`) file:

```
D   ,N   ,X
0   ,5000,8
0.1 ,5002,14
0.25,2008,22
0.5 ,2002,55
0.75,1832,100
1   ,1168,109
1.5 ,562 ,100
2   ,333 ,103
3   ,193 ,108
4   ,103 ,103
5   ,59  ,107
```
