
setClass("matrix_container",representation(training_matrix="matrix.csr", classification_matrix="matrix.csr", training_codes="factor", testing_codes="factor", column_names="vector", virgin="logical"))	
	