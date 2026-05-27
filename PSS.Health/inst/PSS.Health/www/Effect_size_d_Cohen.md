---
title: "D de cohen"
author: "Unidade de Bioestatística"
date: "03/11/2020"
output: html_document
---

O d de Cohen é uma medida da magnitude da diferença entre as médias dos grupos de forma padronizada. Por ser uma medida padronizada, o d de Cohen permite comparar os efeitos obtidos em diferentes estudos que mediram variáveis diferentes ou usaram escalas distintas.


O d de Cohen é definido como


$$
\text{Cohen'd} = \dfrac{\text{Diferença das médias}}{s_{pooled}},
$$

onde 

- $s_{pooled} = \sqrt{ \dfrac{(n_A - 1)s_A^2 + (n_B - 1)s_B^2}{n_A+n_B-2} }$


- $n_A$ é o tamanho amostral do grupo A


- $n_B$ é o tamanho amostral do grupo B


- $s_A$ é o desvio padrão do grupo A


- $s_B$ é o desvio padrão do grupo B.


<br><br>


*Você também pode calcular o d de Cohen na aba 'Outras ferramentas' --> 'd de Cohen'*


<br>


**Pode ser um valor da literatura ou um valor que o pesquisador deseja encontrar e que tenha relevância clínica.**
