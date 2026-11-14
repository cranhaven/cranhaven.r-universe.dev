# ptm 1.0.1

The structure of the package has been substantially simplified reducing multiples third parties dependencies in order to gain in robustness and portability. The focus has been put on the posttranslational modification of methionine. The users interested in the original functionality (extended utilities) still can opt to download one of the older version of the package at https://bitbucket.org/jcaledo/ptm. 

# ptm 0.2.7

1. Added a `NEWS.md` file to track changes to the package.
2. Packages muscle, Biostrings and KEGGREST have been used conditionally.
3. url within the function get.area() has been updated to https://curie.utmb.edu/cgi-bin/getarea.cgi.
4. The structure of the data reported by the function meto.scan() has been simplified.

# ptm 0.2.6

1. compute.dssp() has been deprecated, mkdssp can be used instead.
2. id.features(), pdb.uniprot() and uniprot.pdb() have been re-coded to accommodate the changes introduced into the new UniProt API.
3. get.go() has been sligtly modified to accommodate changes in the EMBL-EBI API.