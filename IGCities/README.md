# Description

This directory has all the functions to simulate the model from Ahlfedlt et al. (2015)

##Outline of Code

##1. Auxiliary functions: utils.R

## 1.1 array_operator.R
**Goal**: an R function to compute matrix operations like Matlab such as .*, ./, -, and +
**Description**: self-explanatory
**Input**: two arrays
**Output**: a new array

## 1.2 sumDims.R
**Goal**: an R function to sum across columns for 3x3 matrixes
**Description**: self-explanatory: useful for a model with multiple sectors
**Input**: a 3x3 Matrix
**Output**: a new vector with the sum of rows, columns, or the third dimension

## 1.3 sumDims.R
**Goal**: an R function to sum across rows or columns and then take the transpose
**Description**: self-explanatory: useful for the basic model
**Input**: a 2x2 Matrix
**Output**: a new vector with the sum of rows or columns

##2. Main functions: basic_models.R

## 2.1 commuting_matrix.R
**Goal**: an R function to transform travel times into commuting costs
**Description**: self-explanatory.
**Input**: a matrix of travel times and the parameter epsilon (how easy is to substitute transportation modes)
**Output**: a matrix of iceberg commuting costs.

## 2.2 av_income.R
**Goal**: an R function to compute the average income in each location
**Description**: self-explanatory.
**Input**: The matrix lambda_ij|i
		   The vector of wages w_j
**Output**: The average income in each location.

## 2.3 wages_inversion.R
**Goal**: an R function to invert the model and recover the wage distribution that matches the model and data
**Description**: self-explanatory.
**Input**: L_j Number of workers in each location
		   L_i Number of residents in each location
		   w_j An initial vector of wages
		   theta: the commuting and migration elasticity
		   tau: iceberg commuting cost matrix
		   tol: tolerance factor
		   nu: convergence parameter
**Output**: w_j: a vector of wages that matches the model with the data
			lambda_ij_i the commuting shares conditional on living in location i
			W_i: a vector of commuter market access measure

## 2.4 density_development.R
**Goal**: an R function to invert the model and recover the density development that matches the model and data
**Description**: self-explanatory.
**Input**: Q_i vector of housing prices
		   K_i size of each location
		   w_j A vector of wages
		   L_j number of workers
		   y_bar: average income 
		   beta: output elasticity w.r.t labor
		   alpha: exp. share in the consumption good
		   mu: land development: output elasticity w.r.t capital
**Output**: varphi_i: a vector of density of the model that matches the model with the data
			Q_mean: the geometric mean of housing prices
			Q_norm: housing prices normalized by the geometric mean
			FS_f: Commercial floorspace
			FS_r: Residential floorspace
			FS: total floorspace = varphi_i x K^{1-mu}

## 2.5 productivity.R
**Goal**: an R function to invert the model and recover the productivity vector
**Description**: self-explanatory.
**Input**: N number of locations
		   Q_i vector of housing prices
		   K_i size of each location
		   w_j A vector of wages
		   L_j number of workers
		   t_ij travel times matrix 
		   delta decay parameter agglomeration
		   lambda strength of the agglomeration force
**Output**: A_i Endogenous TFP measure (takes into account the externalities)
			a_i baseline and exogenous productivity measure

## 2.6 living_amenities.R
**Goal**: an R function to invert the model and recover the amenity vector
**Description**: self-explanatory.
**Input**: N number of locations
		   Q_i vector of housing prices
		   L_i number of residents in each location
		   W_j A vector of commuter market access measures
		   t_ij travel times matrix 
		   theta commuting elasticity
		   alpha exp. share in the consumption good
		   rho decay parameter of congestion forces
		   eta strength of the congestion force
**Output**: B_i Endogenous amenity measure (takes into account the externalities)
			b_i baseline and exogenous amenity measure

## 2.7 inversionModel.R		
**Goal**: an R function to invert the model and recover the economy fundamentals
**Description**: self-explanatory.
**Input**: N number of locations
		   Q_i vector of housing prices
		   L_i number of residents in each location
		   L_j number of workers in each location
		   K size of each location
		   t_ij travel times matrix
		   zeta convergence parameter
		   tol tolerance factor 
		   alpha exp. share in the consumption good
		   beta output elasticity wrt labor
		   theta commuting elasticity
		   delta decay parameter agglomeration
		   rho decay parameter of congestion forces
		   lambda  strength of the agglomeration force	
		   eta strength of the congestion force
		   epsilon paramet travel times to commuting costs
		   mu land development output elasticity wrt capital
**Output**: This function uses all the other function as inputs
			A_i Endogenous TFP measure (takes into account the externalities)
			a_i baseline and exogenous productivity measure
			B_i Endogenous amenity measure (takes into account the externalities)
			b_i baseline and exogenous amenity measure
			w_j vector of wages that match the model with the data
			varphi density of development
			Q_norm housing prices normalized

## 2.7 solveModel.R		
**Goal**: an R function to solve the model and compute counterfactuals
**Description**: self-explanatory.
**Input**: N number of locations
		   L_i initial number of residents in each location
		   K size of each location
		   t_ij travel times matrix
		   a baseline productivity
		   b baseline amenity
		   endo_Lr update residents or not
		   alpha exp. share in the consumption good
		   beta output elasticity wrt labor
		   theta commuting elasticity
		   delta decay parameter agglomeration
		   rho decay parameter of congestion forces
		   lambda  strength of the agglomeration force	
		   eta strength of the congestion force
		   epsilon paramet travel times to commuting costs
		   mu land development output elasticity wrt capital
		   w_eq initial vector of wages
		   Q_eq initial vector of housing prices
		   theta_eq initial share of floorspace used commercially
**Output**: w new vector of wages
			W_i new commuter market access measure
			B new vector of amenities (takes into account the externalities)
			A new vector of productivities (takes into account the externalities)
			Q new vector of floorspace prices
			lambda_ij_i new matrix of commuting shares
			L_i new vector of residents
			L_j new vector of workers
			ybar average income
			ttheta new vector of the share of floorspace used commercially
			u welfare in each location (not considering the shock)
			U aggregate welfare			







