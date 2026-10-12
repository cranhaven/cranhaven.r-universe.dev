#' @details
#'
#' R do vector to vector calculation, but has convention on how vectors
#'	should be recycled or truncated. 
#'	The rule of match objects of different dimensions is simple and
#'	elegance, but sometimes annoying.  MatLab and Python have different
#'	rules doing such match.  Their rules are sometimes convinient but
#'	bug-proning.
#'	package:tensorA is smart in matching the margins, displaying
#'	the usefulness of naming the dimension and do the automatical matching
#'	when operating on two arrays.
#'
# For convinient, *apply() do special
#	map and reduce, in which: apply for array to vector,
#	{l,s,v}apply for vector to vector,
#	mapply for many vector to vector (and utilized by Vectorize()),
#	tapply for typed groups to array, rapply for recursive apply, and
#	eapply for environment apply.
#	outer() for vector x vector to array mapping, but need
#	vectorized FUN(); it actually takes care of the dimensions only.
#	package:Jmisc oapply() for 2 vectors to matrix, built on
#	expand.grid() and mapply.
#
#' In addition, for a jagged (ragged) array R can
#'
#'	1. (like the so called Iliffe vector) have a list,
#'	and each sublist is the members of a group, which is
#'	unconvinient (most math function not accepts list) but most flexible,
#'	running with lapply() (also ref. package:rowr);
#'	2. have a vector (values) recording all members and another (index)
#'	vector recording the grouping, running with tapply() (and by/aggregate);
#'	3. have a matrix where each column/row is for a group and short groups
#'	are filled with placeholder like NA.
#'
#'	Representations of (1) and (2) are inter-convertable via stack/unstack.
#'	Map and reduce for (2) seems handy and can be less flexible than
#'	tapply(), since the grouping is continuous and is repeating,
#'	naturally when members among groups are actually similar.
#'	package:lambda.tools allows block operations but the matching are not
#'	automatically.
#'
#' It is needed to clarify that operation on subdimensions means whether
#'	operating within every group independently (similar to apply(MARGIN))
#'	or operating among groups, maintaining the contain of a group
#'	(apply() achieves this via apply() on the complement margins).
#'	Similarly, should the length of a subdimensions be the sizes of groups
#'	or the number of groups?
#'	I think the first convention is compatible with apply().
#'	Thus when utility functions of this package operate on subdimension, 
#'	they operate within every group independently.
#'	Not like simple arrays that their margins are orthogonal and complement,
#'	for generalized arrays, there is no complement margins for
#'	subdimentions, since the array is ragged.
#'	When some operations concerning comparing among groups, special tricks
#'	are needed (may be I will implement some of these tricks as
#'	utility functions in the future, for example, getting the number of
#'	groups corresponding to a subdimension).
#' 
#' One of the advantages of the library is to do parallel calculation
#'	automatically.  Map and reduce operations have both serial and
#'	parallel implementation.  When `options(mc.cores)` is set to a value
#'	no less than 2, the attaching of garray library will triger
#'	`.onAttach()`, which defined `.LAPPLY` and `.MAPPLY`, which are the
#'	working horse of `amap()` and `areduce()`, by the parallel
#'	implementation.
#'	To double check (not needed by regular user) the activation of
#'	parallel, run
#'	`amap(function(x) Sys.getpid(), garray(1:4, margins="I"), "I")`.
#'	If elements the return are of the same, `amap()` runs in serial;
#'	if they are of different, it runs in parallel.
#'	
#' Design:
#'
#'	* Naming convention:
#'	  * "simple array" - object that is.array() is TRUE;
#'	  * "generalized array" - object that is.garray() is TRUE;
#'	  * "array" - generalized array, especially in issued message.
#'	* In this world, only 2 types of data are welcome: array and scalar.
#'	* Most functions also work for simple array, with warnings.
#'	* Attribute "sdim" of simple array are neglected (since no superdim).
#'	* Attribute class="garray" is almost only for method dispatching.
#'	  The validity of a generalized array in fact depends on the
#'	  correctness of dimnames, which is tested by `is.garray()`.
#
# Vocabulary:
#	Context	code	message		help	
#	array	array	simple array	simple array
#	garray	garray	array		generalized array
#	sdim	sdim	subdim		subdimension
#
# CAVEAT:
#	* R's Native `%*%` multiples arrays (dim>2, and same lengths) as
#	  inner product of vectors, resulting a scalar.
#	* Some R's Native functions (`%*%`/crossprod/apply) do not save values 
#	  of dimnames() if it is NULL even thouth with names (margins).
#	* R's native `dim<-` should always go before `dimnames<-`. 
#	  Whenever `dim<-` is called, attribute `dimnames` gets removed.
#	  `dim.garray<-` tries to save dimnames as many as possible.
#' @keywords internal
#' @aliases garray-package
# If remove @aliases here, roxygen2 will generate '@aliases garray'
#	which conflicts with the help file of function garray().
"_PACKAGE"
#> [1] "_PACKAGE"
