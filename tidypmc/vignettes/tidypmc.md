Introduction to tidypmc
================
Chris Stubben
December 24, 2023

The `tidypmc` package parses XML documents in the Open Access subset of
[Pubmed Central](https://europepmc.org). Download the full text using
`pmc_xml`.

``` r
library(tidypmc)
doc <- pmc_xml("PMC2231364")
doc
#  {xml_document}
#  <article article-type="research-article" xmlns:xlink="http://www.w3.org/1999/xlink">
#  [1] <front>\n  <journal-meta>\n    <journal-id journal-id-type="nlm-ta">BMC Microbiol</journal-id ...
#  [2] <body>\n  <sec>\n    <title>Background</title>\n    <p><italic>Yersinia pestis </italic>is th ...
#  [3] <back>\n  <ack>\n    <sec>\n      <title>Acknowledgements</title>\n      <p>We thank Dr. Chen ...
```

The package includes five functions to parse the `xml_document`.

| R function      | Description                                                                 |
|:----------------|:----------------------------------------------------------------------------|
| `pmc_text`      | Split section paragraphs into sentences with full path to subsection titles |
| `pmc_caption`   | Split figure, table and supplementary material captions into sentences      |
| `pmc_table`     | Convert table nodes into a list of tibbles                                  |
| `pmc_reference` | Format references cited into a tibble                                       |
| `pmc_metadata`  | List journal and article metadata in front node                             |

`pmc_text` splits paragraphs into sentences and removes any tables,
figures or formulas that are nested within paragraph tags, replaces
superscripted references with brackets, adds carets and underscores to
other superscripts and subscripts and includes the full path to the
subsection title.

``` r
library(dplyr)
txt <- pmc_text(doc)
txt
#  # A tibble: 194 × 4
#     section    paragraph sentence text                                                               
#     <chr>          <int>    <int> <chr>                                                              
#   1 Title              1        1 Comparative transcriptomics in Yersinia pestis: a global view of e…
#   2 Abstract           1        1 Background - Environmental modulation of gene expression in Yersin…
#   3 Abstract           1        2 Using cDNA microarray technology, we have analyzed the global gene…
#   4 Abstract           2        1 Results - To provide us with a comprehensive view of environmental…
#   5 Abstract           2        2 Almost all known virulence genes of Y. pestis were differentially …
#   6 Abstract           2        3 Clustering enabled us to functionally classify co-expressed genes,…
#   7 Abstract           2        4 Collections of operons were predicted from the microarray data, an…
#   8 Abstract           2        5 Several regulatory DNA motifs, probably recognized by the regulato…
#   9 Abstract           3        1 Conclusion - The comparative transcriptomics analysis we present h…
#  10 Background         1        1 Yersinia pestis is the etiological agent of plague, alternatively …
#  # ℹ 184 more rows
count(txt, section)
#  # A tibble: 21 × 2
#     section                                                  n
#     <chr>                                                <int>
#   1 Abstract                                                 8
#   2 Authors' contributions                                   6
#   3 Background                                              20
#   4 Conclusion                                               3
#   5 Methods; Clustering analysis                             7
#   6 Methods; Collection of microarray expression data       17
#   7 Methods; Discovery of regulatory DNA motifs              8
#   8 Methods; Gel mobility shift analysis of Fur binding     13
#   9 Methods; Operon prediction                               5
#  10 Methods; Verification of predicted operons by RT-PCR     7
#  # ℹ 11 more rows
```

`pmc_caption` splits figure, table and supplementary material captions
into sentences.

``` r
cap1 <- pmc_caption(doc)
#  Found 5 figures
#  Found 4 tables
#  Found 3 supplements
filter(cap1, sentence == 1)
#  # A tibble: 12 × 4
#     tag        label                       sentence text                                             
#     <chr>      <chr>                          <int> <chr>                                            
#   1 figure     Figure 1                           1 Environmental modulation of expression of virule…
#   2 figure     Figure 2                           1 RT-PCR analysis of potential operons.            
#   3 figure     Figure 3                           1 Schematic representation of the clustered microa…
#   4 figure     Figure 4                           1 Graphical representation of the consensus patter…
#   5 figure     Figure 5                           1 EMSA analysis of the binding of Fur protein to p…
#   6 table      Table 1                            1 Stress-responsive operons in Y. pestis predicted…
#   7 table      Table 2                            1 Classification of the gene members of the cluste…
#   8 table      Table 3                            1 Motif discovery for the clustering genes         
#   9 table      Table 4                            1 Designs for expression profiling of Y. pestis    
#  10 supplement Additional file 1 Figure S1        1 Growth curves of Y. pestis strain 201 under diff…
#  11 supplement Additional file 2 Table S1         1 All the transcriptional changes of 4005 genes of…
#  12 supplement Additional file 3 Table S2         1 List of oligonucleotide primers used in this stu…
```

`pmc_table` formats tables by collapsing multiline headers, expanding
rowspan and colspan attributes and adding subheadings into a new column.

``` r
tab1 <- pmc_table(doc)
#  Parsing 4 tables
#  Adding footnotes to Table 1
sapply(tab1, nrow)
#  Table 1 Table 2 Table 3 Table 4 
#       39      23       4      34
tab1[[1]]
#  # A tibble: 39 × 5
#     subheading                Potential operon (r …¹ `Gene ID` Putative or predicte…² `Reference (s)`
#     <chr>                     <chr>                  <chr>     <chr>                  <chr>          
#   1 Iron uptake or heme synt… yfeABCD operon* (r > … YPO2439-… Transport/binding che… yfeABCD [54]   
#   2 Iron uptake or heme synt… hmuRSTUV operon (r > … YPO0279-… Transport/binding hem… hmuRSTUV [55]  
#   3 Iron uptake or heme synt… ysuJIHG* (r > 0.95)    YPO1529-… Iron uptake            -              
#   4 Iron uptake or heme synt… sufABCDS* (r > 0.90)   YPO2400-… Iron-regulated Fe-S c… -              
#   5 Iron uptake or heme synt… YPO1854-1856* (r > 0.… YPO1854-… Iron uptake or heme s… -              
#   6 Sulfur metabolism         tauABCD operon (r > 0… YPO0182-… Transport/binding tau… tauABCD [56]   
#   7 Sulfur metabolism         ssuEADCB operon (r > … YPO3623-… Sulphur metabolism     ssu operon [57]
#   8 Sulfur metabolism         cys operon (r > 0.92)  YPO3010-… Cysteine synthesis     -              
#   9 Sulfur metabolism         YPO1317-1319 (r > 0.9… YPO1317-… Sulfur metabolism?     -              
#  10 Sulfur metabolism         YPO4109-4111 (r > 0.9… YPO4109-… Sulfur metabolism?     -              
#  # ℹ 29 more rows
#  # ℹ abbreviated names: ¹​`Potential operon (r value)`, ²​`Putative or predicted function`
```

Captions and footnotes are added as attributes.

``` r
attributes(tab1[[1]])
#  $names
#  [1] "subheading"                     "Potential operon (r value)"    
#  [3] "Gene ID"                        "Putative or predicted function"
#  [5] "Reference (s)"                 
#  
#  $row.names
#   [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32
#  [33] 33 34 35 36 37 38 39
#  
#  $class
#  [1] "tbl_df"     "tbl"        "data.frame"
#  
#  $caption
#  [1] "Stress-responsive operons in Y. pestis predicted from microarray expression data"
#  
#  $footnotes
#  [1] "'r' represents the correlation coefficient of adjacent genes; '*' represent the defined operon has the similar expression pattern in two other published microarray datasets [7, 21]; '?' inferred functions of uncharacterized genes; '-' means the corresponding operons have not been experimentally validated in other bacteria."
```

Use `collapse_rows` to join column names and cell values in a semi-colon
delimited string (and then search using functions in the next section).

``` r
collapse_rows(tab1, na.string="-")
#  # A tibble: 100 × 3
#     table     row text                                                                               
#     <chr>   <int> <chr>                                                                              
#   1 Table 1     1 subheading=Iron uptake or heme synthesis; Potential operon (r value)=yfeABCD opero…
#   2 Table 1     2 subheading=Iron uptake or heme synthesis; Potential operon (r value)=hmuRSTUV oper…
#   3 Table 1     3 subheading=Iron uptake or heme synthesis; Potential operon (r value)=ysuJIHG* (r >…
#   4 Table 1     4 subheading=Iron uptake or heme synthesis; Potential operon (r value)=sufABCDS* (r …
#   5 Table 1     5 subheading=Iron uptake or heme synthesis; Potential operon (r value)=YPO1854-1856*…
#   6 Table 1     6 subheading=Sulfur metabolism; Potential operon (r value)=tauABCD operon (r > 0.90)…
#   7 Table 1     7 subheading=Sulfur metabolism; Potential operon (r value)=ssuEADCB operon (r > 0.97…
#   8 Table 1     8 subheading=Sulfur metabolism; Potential operon (r value)=cys operon (r > 0.92); Ge…
#   9 Table 1     9 subheading=Sulfur metabolism; Potential operon (r value)=YPO1317-1319 (r > 0.97); …
#  10 Table 1    10 subheading=Sulfur metabolism; Potential operon (r value)=YPO4109-4111 (r > 0.90); …
#  # ℹ 90 more rows
```

`pmc_reference` extracts the id, pmid, authors, year, title, journal,
volume, pages, and DOIs from reference tags.

``` r
ref1 <- pmc_reference(doc)
#  Found 76 citation tags
ref1
#  # A tibble: 76 × 9
#        id pmid     authors                                      year title journal volume pages doi  
#     <int> <chr>    <chr>                                       <int> <chr> <chr>   <chr>  <chr> <chr>
#   1     1 8993858  Perry RD, Fetherston JD                      1997 Yers… Clin M… 10     35-66 <NA> 
#   2     2 16053250 Hinnebusch BJ                                2005 The … Curr I… 7      197-… <NA> 
#   3     3 6469352  Straley SC, Harmon PA                        1984 Yers… Infect… 45     655-… <NA> 
#   4     4 15557646 Huang XZ, Lindler LE                         2004 The … Infect… 72     7212… 10.1…
#   5     5 15721832 Pujol C, Bliska JB                           2005 Turn… Clin I… 114    216-… 10.1…
#   6     6 12732299 Rhodius VA, LaRossa RA                       2003 Uses… Curr O… 6      114-… 10.1…
#   7     7 15342600 Motin VL, Georgescu AM, Fitch JP, Gu PP, N…  2004 Temp… J Bact… 186    6298… 10.1…
#   8     8 15557737 Han Y, Zhou D, Pang X, Song Y, Zhang L, Ba…  2004 Micr… Microb… 48     791-… <NA> 
#   9     9 15777740 Han Y, Zhou D, Pang X, Zhang L, Song Y, To…  2005 DNA … Microb… 7      335-… 10.1…
#  10    10 15808945 Han Y, Zhou D, Pang X, Zhang L, Song Y, To…  2005 Comp… Res Mi… 156    403-… 10.1…
#  # ℹ 66 more rows
```

Finally, `pmc_metadata` saves journal and article metadata to a list.

``` r
pmc_metadata(doc)
#  $PMCID
#  [1] "PMC2231364"
#  
#  $Title
#  [1] "Comparative transcriptomics in Yersinia pestis: a global view of environmental modulation of gene expression"
#  
#  $Authors
#  [1] "Yanping Han, Jingfu Qiu, Zhaobiao Guo, He Gao, Yajun Song, Dongsheng Zhou, Ruifu Yang"
#  
#  $Year
#  [1] 2007
#  
#  $Journal
#  [1] "BMC Microbiology"
#  
#  $Volume
#  [1] "7"
#  
#  $Pages
#  [1] "96"
#  
#  $`Published online`
#  [1] "2007-10-29"
#  
#  $`Date received`
#  [1] "2007-6-2"
#  
#  $DOI
#  [1] "10.1186/1471-2180-7-96"
#  
#  $PMID
#  [1] "17963531"
#  
#  $Publisher
#  [1] "BioMed Central"
```

## Searching text

There are a few functions to search within the `pmc_text` or collapsed
`pmc_table` output. `separate_text` uses the
[stringr](https://stringr.tidyverse.org/) package to extract any
matching regular expression.

``` r
separate_text(txt, "[ATCGN]{5,}")
#  # A tibble: 9 × 5
#    match                 section                                             paragraph sentence text 
#    <chr>                 <chr>                                                   <int>    <int> <chr>
#  1 ACGCAATCGTTTTCNT      Results and Discussion; Computational discovery of…         2        3 A 16…
#  2 AAACGTTTNCGT          Results and Discussion; Computational discovery of…         2        4 It i…
#  3 TGATAATGATTATCATTATCA Results and Discussion; Computational discovery of…         2        5 A 21…
#  4 GATAATGATAATCATTATC   Results and Discussion; Computational discovery of…         2        6 It i…
#  5 TGANNNNNNTCAA         Results and Discussion; Computational discovery of…         2        7 A 15…
#  6 TTGATN                Results and Discussion; Computational discovery of…         2        8 It i…
#  7 NATCAA                Results and Discussion; Computational discovery of…         2        8 It i…
#  8 GTTAATTAA             Results and Discussion; Computational discovery of…         3        4 The …
#  9 GTTAATTAATGT          Results and Discussion; Computational discovery of…         3        5 An A…
```

A few wrappers search pre-defined patterns and add an extra step to
expand matched ranges. `separate_refs` matches references within
brackets using `\\[[0-9, -]+\\]` and expands ranges like `[7-9]`.

``` r
x <- separate_refs(txt)
x
#  # A tibble: 93 × 6
#        id match section    paragraph sentence text                                                   
#     <dbl> <chr> <chr>          <int>    <int> <chr>                                                  
#   1     1 [1]   Background         1        1 Yersinia pestis is the etiological agent of plague, al…
#   2     2 [2]   Background         1        3 To produce a transmissible infection, Y. pestis coloni…
#   3     3 [3]   Background         1        9 However, a few bacilli are taken up by tissue macropha…
#   4     4 [4,5] Background         1       10 Residence in this niche also facilitates the bacteria'…
#   5     5 [4,5] Background         1       10 Residence in this niche also facilitates the bacteria'…
#   6     6 [6]   Background         2        1 A DNA microarray is able to determine simultaneous cha…
#   7     7 [7-9] Background         2        2 We and others have measured the gene expression profil…
#   8     8 [7-9] Background         2        2 We and others have measured the gene expression profil…
#   9     9 [7-9] Background         2        2 We and others have measured the gene expression profil…
#  10    10 [10]  Background         2        2 We and others have measured the gene expression profil…
#  # ℹ 83 more rows
filter(x, id == 8)
#  # A tibble: 5 × 6
#       id match           section                                             paragraph sentence text 
#    <dbl> <chr>           <chr>                                                   <int>    <int> <chr>
#  1     8 [7-9]           Background                                                  2        2 We a…
#  2     8 [8-13,15]       Background                                                  2        4 In o…
#  3     8 [7-13,15,19-21] Results and Discussion                                      2        1 Rece…
#  4     8 [7-9]           Results and Discussion; Virulence genes in respons…         3        1 As d…
#  5     8 [8-10]          Methods; Collection of microarray expression data           1        6 The …
```

`separate_tags` expands locus tag ranges.

``` r
collapse_rows(tab1, na="-") %>%
  separate_tags("YPO")
#  # A tibble: 270 × 5
#     id      match        table     row text                                                          
#     <chr>   <chr>        <chr>   <int> <chr>                                                         
#   1 YPO2439 YPO2439-2442 Table 1     1 subheading=Iron uptake or heme synthesis; Potential operon (r…
#   2 YPO2440 YPO2439-2442 Table 1     1 subheading=Iron uptake or heme synthesis; Potential operon (r…
#   3 YPO2441 YPO2439-2442 Table 1     1 subheading=Iron uptake or heme synthesis; Potential operon (r…
#   4 YPO2442 YPO2439-2442 Table 1     1 subheading=Iron uptake or heme synthesis; Potential operon (r…
#   5 YPO0279 YPO0279-0283 Table 1     2 subheading=Iron uptake or heme synthesis; Potential operon (r…
#   6 YPO0280 YPO0279-0283 Table 1     2 subheading=Iron uptake or heme synthesis; Potential operon (r…
#   7 YPO0281 YPO0279-0283 Table 1     2 subheading=Iron uptake or heme synthesis; Potential operon (r…
#   8 YPO0282 YPO0279-0283 Table 1     2 subheading=Iron uptake or heme synthesis; Potential operon (r…
#   9 YPO0283 YPO0279-0283 Table 1     2 subheading=Iron uptake or heme synthesis; Potential operon (r…
#  10 YPO1529 YPO1529-1532 Table 1     3 subheading=Iron uptake or heme synthesis; Potential operon (r…
#  # ℹ 260 more rows
```

### Using `xml2`

The `pmc_*` functions use the [xml2](https://github.com/r-lib/xml2)
package for parsing and may fail in some situations, so it helps to know
how to parse `xml_documents`. Use `cat` and `as.character` to view nodes
returned by `xml_find_all`.

``` r
library(xml2)
refs <- xml_find_all(doc, "//ref")
refs[1]
#  {xml_nodeset (1)}
#  [1] <ref id="B1">\n  <citation citation-type="journal">\n    <person-group person-group-type="aut ...
cat(as.character(refs[1]))
#  <ref id="B1">
#    <citation citation-type="journal">
#      <person-group person-group-type="author">
#        <name>
#          <surname>Perry</surname>
#          <given-names>RD</given-names>
#        </name>
#        <name>
#          <surname>Fetherston</surname>
#          <given-names>JD</given-names>
#        </name>
#      </person-group>
#      <article-title>Yersinia pestis--etiologic agent of plague</article-title>
#      <source>Clin Microbiol Rev</source>
#      <year>1997</year>
#      <volume>10</volume>
#      <fpage>35</fpage>
#      <lpage>66</lpage>
#      <pub-id pub-id-type="pmid">8993858</pub-id>
#    </citation>
#  </ref>
```

Many journals use superscripts for references cited so they usually
appear after words like `results9` below.

``` r
# doc1 <- pmc_xml("PMC6385181")
doc1 <- read_xml(system.file("extdata/PMC6385181.xml", package = "tidypmc"))
gsub(".*\\. ", "", xml_text( xml_find_all(doc1, "//sec/p"))[2])
#  [1] "RNA-seq identifies the most relevant genes and RT-qPCR validates its results9, especially in the field of environmental and host adaptation10,11 and antimicrobial response12."
```

Find the tags using `xml_find_all` and then update the nodes by adding
brackets or other text.

``` r
bib <- xml_find_all(doc1, "//xref[@ref-type='bibr']")
bib[1]
#  {xml_nodeset (1)}
#  [1] <xref ref-type="bibr" rid="CR1">1</xref>
xml_text(bib) <- paste0(" [", xml_text(bib), "]")
bib[1]
#  {xml_nodeset (1)}
#  [1] <xref ref-type="bibr" rid="CR1"> [1]</xref>
```

The text is now separated from the reference. Note the `pmc_text`
function adds the brackets by default.

``` r
gsub(".*\\. ", "", xml_text( xml_find_all(doc1, "//sec/p"))[2])
#  [1] "RNA-seq identifies the most relevant genes and RT-qPCR validates its results [9], especially in the field of environmental and host adaptation [10], [11] and antimicrobial response [12]."
```

Genes, species and many other terms are often included within italic
tags. You can mark these nodes using the same code above or simply list
all the names in italics and search text or tables for matches, for
example three letter gene names in text below.

``` r
library(tibble)
x <- xml_name(xml_find_all(doc, "//*"))
tibble(tag=x) %>%
  count(tag, sort=TRUE)
#  # A tibble: 84 × 2
#     tag               n
#     <chr>         <int>
#   1 td              398
#   2 given-names     388
#   3 name            388
#   4 surname         388
#   5 italic          235
#   6 pub-id          129
#   7 tr              117
#   8 xref            108
#   9 year             80
#  10 article-title    77
#  # ℹ 74 more rows
it <- xml_text(xml_find_all(doc, "//sec//p//italic"), trim=TRUE)
it2 <- tibble(italic=it) %>%
  count(italic, sort=TRUE)
it2
#  # A tibble: 53 × 2
#     italic              n
#     <chr>           <int>
#   1 Y. pestis          46
#   2 in vitro            5
#   3 E. coli             4
#   4 psaEFABC            3
#   5 r                   3
#   6 Yersinia            2
#   7 Yersinia pestis     2
#   8 cis                 2
#   9 fur                 2
#  10 n                   2
#  # ℹ 43 more rows
filter(it2, nchar(italic) == 3)
#  # A tibble: 8 × 2
#    italic     n
#    <chr>  <int>
#  1 cis        2
#  2 fur        2
#  3 cys        1
#  4 hmu        1
#  5 ybt        1
#  6 yfe        1
#  7 yfu        1
#  8 ymt        1
separate_text(txt, c("fur", "cys", "hmu", "ybt", "yfe", "yfu", "ymt"))
#  # A tibble: 9 × 5
#    match section                                                             paragraph sentence text 
#    <chr> <chr>                                                                   <int>    <int> <chr>
#  1 ymt   Results and Discussion; Virulence genes in response to multiple en…         3        4 The …
#  2 fur   Results and Discussion; Clustering analysis and functional classif…         3        2 It i…
#  3 yfe   Results and Discussion; Clustering analysis and functional classif…         3        4 Gene…
#  4 hmu   Results and Discussion; Clustering analysis and functional classif…         3        4 Gene…
#  5 yfu   Results and Discussion; Clustering analysis and functional classif…         3        4 Gene…
#  6 ybt   Results and Discussion; Clustering analysis and functional classif…         3        4 Gene…
#  7 cys   Results and Discussion; Clustering analysis and functional classif…         4        2 Gene…
#  8 cys   Results and Discussion; Clustering analysis and functional classif…         4        3 Clus…
#  9 fur   Methods; Gel mobility shift analysis of Fur binding                         1        1 The …
```
