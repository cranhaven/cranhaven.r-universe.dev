# *INet* for Network Integration

<img src="https://github.com/ValeriaPolicastro/Paper-Robin/blob/master/images/logoINet.png" align="right" width="150" height="150"/> The method is described in detail in the paper:

*V. Policastro, M. Magnani, C. Angelini and A. Carissimo(2024). INet for network integration. Computational Statistics.* <https://link.springer.com/article/10.1007/s00180-024-01536-8>

For the installation in R:

```         
           devtools::install_github("ValeriaPolicastro/INet-Tool")
```

We want to identify general information by building a **Consensus Network** and then to obtain **Case-Specific Networks** one for each layer with the information present only in that layer and not in all the others.

<p align="center">

<img src="https://github.com/ValeriaPolicastro/Images/blob/master/images/Idea2.png" width="480" height="145"/>

</p>

*Important Functions:*

-   **consensusNet** to create the *Consensus Network*

-   **specificNet** to create the *Case Specific Network*

-   **thresholdNet** to change the threshold of the consensus from the similar layer after the algorithm

-   **JWmean** to see how distant are the layers at the beginning

*Integration Network Algorithm:*

<p align="center">

<img src="https://github.com/ValeriaPolicastro/Images/blob/master/images/Algorithm2.png" width="600" height="300"/>

</p>
