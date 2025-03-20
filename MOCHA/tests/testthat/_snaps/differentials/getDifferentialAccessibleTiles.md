# getDifferentialAccessibleTiles works on a 3 sample test dataset

    GRanges object with 11360 ranges and 13 metadata columns:
              seqnames              ranges strand |                   Tile
                 <Rle>           <IRanges>  <Rle> |            <character>
          [1]    chr10 100080500-100080999      * | chr10:100080500-1000..
          [2]    chr10 100147000-100147499      * | chr10:100147000-1001..
          [3]    chr10 100227000-100227499      * | chr10:100227000-1002..
          [4]    chr10 101279500-101279999      * | chr10:101279500-1012..
          [5]    chr10 101380500-101380999      * | chr10:101380500-1013..
          ...      ...                 ...    ... .                    ...
      [11356]     chrX   99870000-99870499      * | chrX:99870000-99870499
      [11357]     chrY   16263500-16263999      * | chrY:16263500-16263999
      [11358]     chrY   21154500-21154999      * | chrY:21154500-21154999
      [11359]     chrY     2709500-2709999      * |   chrY:2709500-2709999
      [11360]     chrY     2803000-2803499      * |   chrY:2803000-2803499
              CellPopulation     Foreground          Background   P_value
                 <character>    <character>         <character> <numeric>
          [1]             C3 scATAC_BMMC_R1 scATAC_CD34_BMMC_R1        NA
          [2]             C3 scATAC_BMMC_R1 scATAC_CD34_BMMC_R1        NA
          [3]             C3 scATAC_BMMC_R1 scATAC_CD34_BMMC_R1        NA
          [4]             C3 scATAC_BMMC_R1 scATAC_CD34_BMMC_R1        NA
          [5]             C3 scATAC_BMMC_R1 scATAC_CD34_BMMC_R1        NA
          ...            ...            ...                 ...       ...
      [11356]             C3 scATAC_BMMC_R1 scATAC_CD34_BMMC_R1        NA
      [11357]             C3 scATAC_BMMC_R1 scATAC_CD34_BMMC_R1        NA
      [11358]             C3 scATAC_BMMC_R1 scATAC_CD34_BMMC_R1        NA
      [11359]             C3 scATAC_BMMC_R1 scATAC_CD34_BMMC_R1        NA
      [11360]             C3 scATAC_BMMC_R1 scATAC_CD34_BMMC_R1        NA
              Test_Statistic       FDR  Log2FC_C  MeanDiff Avg_Intensity_Case
                   <numeric> <logical> <numeric> <numeric>          <numeric>
          [1]             NA      <NA>        NA        NA                 NA
          [2]             NA      <NA>        NA        NA                 NA
          [3]             NA      <NA>        NA        NA                 NA
          [4]             NA      <NA>        NA        NA                 NA
          [5]             NA      <NA>        NA        NA                 NA
          ...            ...       ...       ...       ...                ...
      [11356]             NA      <NA>        NA        NA                 NA
      [11357]             NA      <NA>        NA        NA                 NA
      [11358]             NA      <NA>  0.966263        NA            14.7355
      [11359]             NA      <NA>        NA        NA                 NA
      [11360]             NA      <NA>        NA        NA                 NA
              Pct0_Case Avg_Intensity_Control Pct0_Control
              <numeric>             <numeric>    <numeric>
          [1]         1               11.3103            0
          [2]         1               11.8950            0
          [3]         1               11.8950            0
          [4]         1               11.3103            0
          [5]         1               16.0911            0
          ...       ...                   ...          ...
      [11356]         1               11.8950            0
      [11357]         1               12.3100            0
      [11358]         0               13.7692            0
      [11359]         1               11.8950            0
      [11360]         1               13.3098            0
      -------
      seqinfo: 24 sequences from an unspecified genome; no seqlengths

