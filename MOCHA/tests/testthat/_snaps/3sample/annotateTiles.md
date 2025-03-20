# annotateTiles works on a 3 sample test dataset

    Code
      SummarizedExperiment::rowRanges(STM)
    Output
      GRanges object with 22773 ranges and 4 metadata columns:
                                  seqnames              ranges strand |        C2
                                     <Rle>           <IRanges>  <Rle> | <logical>
        chr10:100028000-100028499    chr10 100028000-100028499      * |      TRUE
        chr10:100080500-100080999    chr10 100080500-100080999      * |     FALSE
        chr10:100147000-100147499    chr10 100147000-100147499      * |     FALSE
        chr10:100227000-100227499    chr10 100227000-100227499      * |     FALSE
        chr10:101190000-101190499    chr10 101190000-101190499      * |      TRUE
                              ...      ...                 ...    ... .       ...
           chrX:99870000-99870499     chrX   99870000-99870499      * |     FALSE
           chrY:16263500-16263999     chrY   16263500-16263999      * |     FALSE
           chrY:21154500-21154999     chrY   21154500-21154999      * |     FALSE
             chrY:2709500-2709999     chrY     2709500-2709999      * |     FALSE
             chrY:2803000-2803499     chrY     2803000-2803499      * |     FALSE
                                         C3    tileType        Gene
                                  <logical> <character> <character>
        chr10:100028000-100028499     FALSE    Promoter       LOXL4
        chr10:100080500-100080999      TRUE      Distal        <NA>
        chr10:100147000-100147499      TRUE  Intragenic     PYROXD2
        chr10:100227000-100227499      TRUE  Intragenic       HPSE2
        chr10:101190000-101190499     FALSE    Promoter        GOT1
                              ...       ...         ...         ...
           chrX:99870000-99870499      TRUE      Distal        <NA>
           chrY:16263500-16263999      TRUE      Distal        <NA>
           chrY:21154500-21154999      TRUE    Promoter        CD24
             chrY:2709500-2709999      TRUE    Promoter      RPS4Y1
             chrY:2803000-2803499      TRUE    Promoter         ZFY
        -------
        seqinfo: 24 sequences from an unspecified genome; no seqlengths

# annotateTiles works on a GRanges

    Code
      ranges
    Output
      GRanges object with 22773 ranges and 4 metadata columns:
                                  seqnames              ranges strand |        C2
                                     <Rle>           <IRanges>  <Rle> | <logical>
        chr10:100028000-100028499    chr10 100028000-100028499      * |      TRUE
        chr10:100080500-100080999    chr10 100080500-100080999      * |     FALSE
        chr10:100147000-100147499    chr10 100147000-100147499      * |     FALSE
        chr10:100227000-100227499    chr10 100227000-100227499      * |     FALSE
        chr10:101190000-101190499    chr10 101190000-101190499      * |      TRUE
                              ...      ...                 ...    ... .       ...
           chrX:99870000-99870499     chrX   99870000-99870499      * |     FALSE
           chrY:16263500-16263999     chrY   16263500-16263999      * |     FALSE
           chrY:21154500-21154999     chrY   21154500-21154999      * |     FALSE
             chrY:2709500-2709999     chrY     2709500-2709999      * |     FALSE
             chrY:2803000-2803499     chrY     2803000-2803499      * |     FALSE
                                         C3    tileType        Gene
                                  <logical> <character> <character>
        chr10:100028000-100028499     FALSE      Distal        <NA>
        chr10:100080500-100080999      TRUE  Intragenic        CPN1
        chr10:100147000-100147499      TRUE      Distal        <NA>
        chr10:100227000-100227499      TRUE  Intragenic        CHUK
        chr10:101190000-101190499     FALSE  Intragenic   LINC01514
                              ...       ...         ...         ...
           chrX:99870000-99870499      TRUE      Distal        <NA>
           chrY:16263500-16263999      TRUE      Distal        <NA>
           chrY:21154500-21154999      TRUE      Distal        <NA>
             chrY:2709500-2709999      TRUE  Intragenic        CD99
             chrY:2803000-2803499      TRUE      Distal        <NA>
        -------
        seqinfo: 24 sequences from an unspecified genome; no seqlengths

