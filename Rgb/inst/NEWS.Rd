\name{Rgb-news}
\title{Rgb package NEWS}
\section{Changes in version 1.7.5 (2023-08-18)}{
  \itemize{
    \item{Layout computation for browsePlot() is now a drawable.list method, to facilitate custom layouts.}
    \item{panelSide argument added to browsePlot().}
    \item{hsFeatures dataset split into hsGenes and hsBands.}
    \item{Minor fixes suggested by R CMD check.}
    \item{C code corrections suggested by Tomas Kalibera (rchk).}
    \item{Article reference added to DESCRIPTION AND CITATION files.}
  }
}
\section{Changes in version 1.7.4 (2020-06-11)}{
  \itemize{
    \item{'customLayout' argument in browsePlot() can now be used to return the default layout.}
    \item{track.exons.GENCODE() is now referenced in 'Annotation' help page.}
  }
}
\section{Changes in version 1.7.3 (2019-06-17)}{
  \itemize{
    \item{Default border color for class track.CNV fixed.}
    \item{'spacing' argument in draw.boxes() can now be a column name.}
  }
}
\section{Changes in version 1.7.2 (2019-03-21)}{
  \itemize{
    \item{'group' argument added to segMerge().}
    \item{Automatic 'ylim' now handled by draw.hist().}
    \item{track.exons.GENCODE() function added.}
    \item{'ylab.horiz' argument added to draw.bg().}
  }
}
\section{Changes in version 1.7.1 (2018-11-21)}{
  \itemize{
    \item{draw.density() added.}
    \item{'normalize.y' argument added to draw.boxes().}
    \item{'colorVal' / 'colorFun' graphical parameters replaced by 'fillColor'.}
    \item{'border' graphical parameter can now be a function.}
    \item{'labelColor' graphical parameter added.}
    \item{'fg' added to draw.steps().}
    \item{'fg' now used for group labels and bonds in draw.boxes().}
  }
}
\section{Changes in version 1.7.0 (2018-07-19)}{
  \itemize{
    \item{'tkrplot' rendering no longer supported in tk.browse().}
    \item{'png.height' and vertical scrollbar added to tk.browse().}
    \item{Better handling of focus and shortcuts in tk.browse().}
    \item{draw.boxes() no longer crashes when there is space for no label.}
  }
}
\section{Changes in version 1.6.1 (2018-07-11)}{
  \itemize{
    \item{Axis control arguments added to draw.bg().}
    \item{Shifted labelling in draw.boxes() when some are masked fixed.}
    \item{Calls to unnamed list elements fixed for future R version compatibility.}
    \item{Fixed the example URL in track.genes.NCBI() man page.}
    \item{'bg', 'bg.inner' and 'fg' added to control background / foreground colors.}
    \item{Mixes of tracks with and without panels no longer crash browsePlot() with par(new=TRUE).}
  }
}
\section{Changes in version 1.6.0 (2017-05-22)}{
  \itemize{
    \item{drawable class now implements drawPanel(), used by browsePlot().}
    \item{tk.browse() now only warns about inconsistent chromosome lists.}
    \item{Page-up and Page-down keys now allows to switch chromosome in tk.browse().}
    \item{Drag-and-drop zoom in tk.browse() now shows a live rectangle.}
    \item{'colorFun' fixed for class track.CNV.}
    \item{'.chromosomes' argument is now handled properly by track.table.GTF().}
  }
}
\section{Changes in version 1.5.1 (2017-04-20)}{
  \itemize{
    \item{tk.browse() error message on Windows with R < 3.4.0 fixed.}
    \item{tk.browse() no longer requires tkrplot with render = 'png'.}
    \item{draw.hist() now accepts a column name as 'origin'.}
  }
}
\section{Changes in version 1.5.0 (2017-03-03)}{
  \itemize{
    \item{Better rendering for plots in tk.browse() for Linux and/or R >= 3.4.0.}
    \item{Better automatic sizing of the plot in tk.browse().}
    \item{All reference class manual pages are now up to date.}
    \item{Non-base package functions are now properly identified, to comply with R CMD check.}
    \item{External() C entry points are now registered, to comply with R CMD check.}
    \item{BAM related functions to longer use .Internal(), to comply with R CMD check.}
    \item{track.bam$crawl() now uses dedidcated environments, to comply with R CMD check.}
    \item{track.bam$summary() stabilized, unaligned BAM files are no more handled.}
    \item{track.bam$extract() return objects have now a proper print method.}
    \item{Welcome message added in tk.browse().}
    \item{tk.browse() no longer display errors when resizing an empty plot.}
    \item{No more warning in tk.browse() when 'maxDepth' is reached by some track.}
    \item{drawable.list$add() compatibility can now be extended to other files and classes.}
    \item{singlePlot() default output looks nicer.}
    \item{Vignette styling update.}
    \item{track.table$groupByPosition() no longer crash when using a factor for grouping.}
    \item{'fuzzyness' argument corrected to 'fuzziness' in crossable$cross() method.}
    \item{Usage lines truncation to nicer display in manual pages.}
    \item{Interactive window grouping fixed for tk.tracks(), tk.memory(), tk.files().}
    \item{'Add from memory' now handles variable names with special characters.}
  }
}
\section{Changes in version 1.4.3 (2016-01-13)}{
  \itemize{
    \item{groupLwd argument added to draw.boxes().}
    \item{'labelCex' default in draw.boxes() and draw.steps() turned back to 1, to avoid visual errors in tk.browse().}
    \item{Y axis sub-title scaled back to cex=1, to avoid visual errors in tk.browse().}
    \item{refTable$setColNames() bug fixed (column bindings were not updated).}
    \item{track.table$setColNames() overloaded accordingly (keep row ordered).}
  }
}
\section{Changes in version 1.4.2 (2015-09-11)}{
  \itemize{
    \item{tk.file(), tk.files(), tk.folder() and handle() are now documented and exported.}
    \item{Collision in draw.boxes() is now handled by an exported yline() function.}
    \item{draw.steps() added.}
  }
}
\section{Changes in version 1.4.1 (2015-05-19)}{
  \itemize{
    \item{Fixed error in draw.boxes() when labels are vertical and plot area too narrow.}
    \item{'xgrid' argument added to draw.bg().}
    \item{'yaxp' argument added to draw.bg().}
    \item{'las' argument added to draw.bg().}
    \item{'new' argument added to draw.bg().}
    \item{singlePlot() rewritten to handle any drawable object.}
    \item{drawable.list$getChromEnd() method added, now used by browsePlot().}
    \item{'colorFun' drawing parameter for track.CNV class updated to comply with last DGV release.}
    \item{segOverlap() performances highly enhanced when factor columns are involved.}
  }
}
\section{Changes in version 1.4.0 (2015-02-15)}{
  \itemize{
    \item{track.fasta class added, with corresponding constructors.}
    \item{draw.seq() function added.}
    \item{Minor DESCRIPTION file updates to comply with new CRAN policies}
  }
}
\section{Changes in version 1.3.0 (2014-10-26)}{
  \itemize{
    \item{Rsamtools dependency replaced by a native R solution.}
    \item{track.bam$crawl() method and derived depth(), extract() and pileup() methods added.}
    \item{track.bam$getChromEnd() now relies on BAM header LN tag to return something.}
    \item{track.bam$coverage() and track.bam$summary() methods added.}
    \item{findDrawables() function added, tk.tracks() and tk.browse() can now interactively import objects from R memory.}
    \item{'Find' button in tk.browse() now properly interprets factor levels ('X' was returned as 23, etc).}
    \item{tk.browse() now returns the reference to the drawable.list objects in use, allowing simultaneous CLI and GUI accesses (see tk.browse() help page).}
    \item{Fix added in track.table$check(), initialize() and show() to call refTable and drawable inherited methods, not only refTable's ones.}
    \item{tools:::RdTags call in helpArgument() abandonned as now raising warning during check.}
    \item{segMerge() and segOverlap() functions and corresponding track.table methods added.}
  }
}
\section{Changes in version 1.2.0 (2014-10-17)}{
  \itemize{
    \item{track.table$size() method and corresponding 'sizetrack' slot added to improve crossable$cross(type='count') performances.}
    \item{readRDT() now systematically rebuild track.table's calls and indexes, for better backward compatibility.}
    \item{Error while calling drawable$fix.param() in global environment fixed.}
    \item{draw.points() now relies on points() rather than plot().}
    \item{'labelOverflow' argument added to draw.boxes().}
    \item{'labelFamily' argument added to draw.boxes(), handling also Hershey vectorial fonts.}
    \item{draw.boxes()' labels are now more respectuous of label tag sizes (especially with 'Hershey' and 'mono' families).}
    \item{draw.boxes()' labels now comply with labelAdj even when labelSrt=90.}
    \item{Better handling of out-of-sight labels in draw.boxes().}
    \item{draw.hist() now draws small bars ahead high bars.}
    \item{'quiet' argument added to cross$crossable().}
    \item{Vignette's title page and margins edited.}
    \item{buildGroupSize() and buildGroupPosition() moved from track.exons to track.table classes.}
    \item{buildGroupSize() and buildGroupPosition() argument defaults for groupSize and groupPosition updated.}
    \item{track.exons class defaults for groupSize and groupPosition drawing parameters updated.}
    \item{track.exons.CCDS() output column names for groupSize and groupPosition updated.}
    \item{draw.boxes() now returns a proper error message if groupSize or groupPosition columns are missing.}
  }
}
\section{Changes in version 1.1.0 (2014-07-23)}{
  \itemize{
    \item{Dependency to R 3.0.0 or above highlighted.}
    \item{'alphaOrder' and 'alphaMin' arguments added to draw.pileup().}
    \item{'colorFun' no longer uses arguments but inherits the calling environment in draw.points(), draw.hist() and draw.points().}
    \item{track.table() based constructors no longer hijack 'warn' argument.}
    \item{draw.boxes() no longer misses boxes at high zoom level.}
    \item{tk.tracks() relooking.}
    \item{tk.edit() now prints some help on drawing parameters (extracted from R help pages).}
    \item{track.bam$slice() arguments are now handled as drawing parameters.}
    \item{track.bam$slice() argument 'addChr' added (defaults to TRUE, previous behavior).}
    \item{track.bam now enforces default ylab and ysub drawing parameters.}
  }
}
\section{Changes in version 1.0.0 (2013-03-07)}{
  \itemize{
    \item{Version reviewed for Bioinformatics publication.}
    \item{Reference class help messages reformatted.}
    \item{Reference class manual pages automatically rebuilt from help messages.}
  }
}
\section{Changes in version 0.8.0 (2013-03-03)}{
  \itemize{
    \item{"How-to" vignette expanded with two user cases.}
    \item{refTable$extract() and track.table$slice() now propose the 'asObject' argument.}
    \item{"tracks.c" no longer relies on deprecated "Rdefines.h".}
    \item{track.table$slice() and derivates now properly handle factor 'chrom' arguments.}
    \item{draw.boxes() no longer bugs with partially displayed groups on the reverse strand.}
    \item{"ATM.bam" external dataset added.}
    \item{"Agilent_4x44k_chr21.gtf" external dataset added.}
    \item{refTable$levels() method added.}
    \item{refTable$metaFields() method added.}
    \item{refTable$addColumn() no longer accepts 1D arrays.}
    \item{track.exons$buildGroupSize() and buildGroupPosition() methods added.}
    \item{read.gtf() better complies with the GTF specification, using regular expressions.}
    \item{Error message rephrased in track.table() constructor.}
    \item{Updated package's DESCRIPTION link.}
  }
}
\section{Changes in version 0.7.1 (2013-12-19)}{
  \itemize{
    \item{tkgrid calls moved to avoid missing elements in Windows interfaces.}
    \item{1x1 size enforced for auto-sized tk.browse plots.}
    \item{Hot keys added to tk.browse() for the main buttons.}
  }
}
\section{Changes in version 0.7.0 (2013-12-10)}{
  \itemize{
    \item{track.lone class renamed into track.table, for better comprehension.}
    \item{Annotation track constructors renamed for better comprehension.}
    \item{Error message for NA end in subtrack corrected.}
    \item{drawable$draw() and sliceable$draw() now handle NA start and end.}
    \item{'id' column in the track.exons class and track.exons.CCDS() constructor renamed into 'transcript'.}
    \item{'position' and 'size' columns added to track.exons.CCDS() output for exon numbering.}
    \item{draw.boxes() no longer hide out-of-range features in groups, relying on 'groupPosition' and 'groupSize' arguments.}
    \item{draw.boxes() no longer crash when grouping features on a factor column.}
    \item{show() and getPath() methods added to the track.bam class.}
    \item{track.bam no longer raise an error when ending position is not provided to draw.}
    \item{track.bam no longer raise an error when drawing an empty region.}
    \item{track.bam no longer raise an error when loaded from a previous session.}
  }
}
\section{Changes in version 0.6.0 (2013-09-02)}{
  \itemize{
    \item{'name' field added to the drawable class.}
    \item{drawable$check() and drawable$show() added for consistency.}
    \item{track.part class removed, as misleading and no more distinct from refTable.}
    \item{drawable.list class added, to replace tk.tracks() in tk.browse() and browsePlot() input.}
    \item{drawable.list and drawable can now be edited interactively using their fix.param() methods.}
    \item{tk.edit() now uses drawable.list objects as input and output.}
    \item{'drawFun' functions for 'sliceable' objects now require a '...' argument, nothing else.}
    \item{track.lone$buildIndex() now produces a check-proof index for empty tables.}
    \item{track.lone$addArms() no longer crashes in already-armed objects.}
  }
}
\section{Changes in version 0.5.1 (2013-08-30)}{
  \itemize{
    \item{draw.boxes() argument 'labelAdj' has new default : "center".}
    \item{draw.boxes() now handles collision when 'labelAdj' is not "left".}
    \item{draw.boxes() argument 'groupBy' added.}
    \item{track.CCDS_exons() and track.exons class updated to use new draw.boxes() 'groupBy' argument.}
  }
}
\section{Changes in version 0.5.0 (2013-08-06)}{
  \itemize{
    \item{tk.browse() no longer stops on browsePlot() warnings.}
    \item{drawable$defaultParams() and drawable$getParam() '...' argument added, to handle 'drawFun' default in sliceable.}
    \item{track.MGC_genes() removed, as track.NCBI_genes() is more reliable for the same purpose.}
    \item{track.lone$buildIndex() now preserves index names (broken since 0.1.5).}
    \item{track.lone$majCalls() becomes track.lone$buildCalls() for consistency.}
    \item{saveRDT() now stores the package version used to generate the file, readRDT() can return this value.}
    \item{draw.hist(), draw.boxes() and draw.points() 'colorFun' now accept a '...' argument for interoperability.}
    \item{track.bands 'height' default replaced from "2 cm" to "3 cm".}
    \item{Right-click on a coordinate field in tk.browse() now empties and selects the field.}
    \item{drawable$chromosomes() virtual method added (currently implemented by track.lone).}
    \item{tk.browse() track spinbox replaced by a combobox.}
    \item{tk.browse() chromosome entry replaced by a combobox.}
    \item{tk.browse() 'Find' can also be triggered by the 'return' key when the text field is selected.}
    \item{tk.browse() 'Jump' can also be triggered by the 'return' key when a coordinate field is selected.}
    \item{draw.boxes() now adds a short space behind box labels.}
    \item{draw.boxes() default box color changed for a lighter grey.}
    \item{tk.edit() track spinbox replaced by a combobox.}
    \item{tk.edit() parameter spinbox replaced by a combobox.}
    \item{tk.edit() no longer complains about "" parameter.}
  }
}
\section{Changes in version 0.4.1 (2013-08-02)}{
  \itemize{
    \item{track.DGV_cnv() now makes use of the '.chromosomes' argument as other annotation functions.}
  }
}
\section{Changes in version 0.4.0 (2013-07-26)}{
  \itemize{
    \item{"colorFun" defaults for draw.hist(), draw.points() and draw.boxes() now take arguments.}
    \item{track.DGV1_cnv() removed, as input files are no more available. track.DGV2_cnv() becomes track.DGV_cnv().}
    \item{"How-to guide to Rgb" vignette added, and few manual pages updated.}
    \item{read.gff3() replaced by read.gtf() and track.GTF().}
    \item{track.genes, track.bands, track.exons and track.CNV classes and constructors added.}
    \item{"Annotation" functions now rely on classes for custom representations.}
    \item{All the "Annotation" function arguments are reorganized.}
  }
}
\section{Changes in version 0.3.1 (2013-07-20)}{
  \itemize{
    \item{ISMB-ECCB 2013 pre-release.}
    \item{normalizePath() calls added to track.bam constructor.}
    \item{initialize() method added to drawable for 'parameters'.}
    \item{Rsamtools dependency made optional, only required by the track.bam() constructor.}
    \item{cghRA specifics removed (model, cghRA.array building), waiting a more general implementation.}
  }
}
\section{Changes in version 0.3.0 (2013-07-12)}{
  \itemize{
    \item{The 'cghRA.track' package becomes 'Rgb', and stand-alone.}
    \item{drawable$defaultParams() new method replaces drawable$defParam() and default().}
    \item{drawable$callParams() method added, to simplify draw method overload.}
    \item{crossable virtual class added, taking the cross() method from sliceable.}
    \item{track.bam class and draw.pileup function added.}
    \item{'drawFun' graphical parameter replaces 'drawMode' and 'additional', allowing multiple function names.}
    \item{saveRDT() no longer uses the global environment for temporary storage (NOTE in 'R CMD check').}
  }
}
\section{Changes in version 0.2.0 (2013-06-10)}{
  \itemize{
    \item{refTable$fill() and refTable$addList() now raise error when classes does not strictly match.}
    \item{refTable$fill() and refTable$addList() now merge factors with identical levels.}
    \item{refTable$addEmptyRows() no longer drops factor class and levels.}
    \item{refTable$coerce() added to replace refTable$fill() misuses.}
    \item{track.lone$addList() now calls track.lone$rowOrder() to stay a valid object.}
    \item{track.lone$check() now also checks for consistency between 'chrom' levels and the index.}
    \item{singlePlot() added.}
  }
}
\section{Changes in version 0.1.5 (2013-05-28)}{
  \itemize{
    \item{Show column updates in tk.tracks() are no longer ignored.}
    \item{The model window should no longer pop afront in tk.browse().}
    \item{'updateLimit' argument added to tk.browse().}
    \item{track.lone$buildIndex() now enforces the integer class, for full NA indexes.}
    \item{track.lone() now warns about chrom turned into NA as not in .chromosomes.}
    \item{subtrack() and checktrack() now handle empty tables.}
  }
}
\section{Changes in version 0.1.4 (2013-05-16)}{
  \itemize{
    \item{browsePlot() and tk.browse() now rely on tk.tracks() for track and design management.}
    \item{tk.browse() now offers a pop-up window with model.test() results for cghRA.regions objects.}
  }
}
\section{Changes in version 0.1.3 (2013-05-06)}{
  \itemize{
    \item{Tk windows names and titles fixed on Gnome 3.}
    \item{Icons replaced by 16x16 and 32x32 GIF images.}
    \item{Tk style 'clam' is the new default on unix platforms.}
    \item{Tk file selection windows become transients.}
    \item{tk.browse() is now also full window sized on unix platforms.}
    \item{tk.browse()'s plot area is now filled and auto scaled at launch.}
    \item{X11 warnings silenced in tk.file() to fix 'X11 BadDrawable'.}
    \item{draw.hist()'s 'colorFun' now takes 'track' and 'origin' as arguments.}
    \item{cghRA.copies default drawing parameter 'colorFun' relies now on copies.}
    \item{Type 'cytoband' added to sliceable$cross method.}
    \item{'mergeArms' and 'splitArms' renamed into 'eraseArms' and 'addArms' to avoid confusion.}
  }
}
\section{Changes in version 0.1.2 (2012-12-19)}{
  \itemize{
	\item{Coordinates inversions in tk.browse() fixed.}
	\item{Help page of browsePlot() fixed.}
	\item{Track editing when no track is selected handled by an error message in tk.browse().}
	\item{drawable$setParam() can now be used to reset a parameter to its default value.}
	\item{tk.edit() can now reset parameters to their default values.}
	\item{drawable$setParam() does not allow anymore a parameter to be set to NULL.}
	\item{'additionnal' and 'colorFun' default values replaced by null functions.}
	\item{default() called without argument now returns all the parameters, not only their names.}
	\item{tk.edit() now distincts custom and default values for parameters.}
  }
}
\section{Changes in version 0.1.1 (2012-12-17)}{
  \itemize{
	\item{draw.bg() acuraccy problems in y axis resolved (workaround).}
	\item{tk.browse() acuraccy for zooming/unzooming sequences resolved (workaround).}
  }
}
\section{Changes in version 0.1.0 (2012-12-14)}{
  \itemize{
    \item{'origin' argument added to draw.hist().}
    \item{'temp' argument added to track.lone methods 'mergeArms' and 'splitArms'.}
    \item{'bty' argument added to draw.bg(), draw.hist(), draw.boxes() and draw.points().}
    \item{browsePlot() no longer draws the last track when 'xaxt' was 'n'.}
    \item{browsePlot() no longer uses 'mar' via '...' for the last track.}
    \item{browsePlot() no longer has side effects when merging arms.}
    \item{TK file choosing windows now remind last directory browsed.}
    \item{require() calls turned into library() ones to raise errors.}
	\item{track.lone$splitArms() now accepts a band track as 'centromeres'.}
	\item{drawable$getParameters() replaced by drawable$defParam() for simpler default overload.}
	\item{draw.boxes() time optimization relying on subtrack().}
	\item{'maxDepth' argument added to draw.boxes().}
	\item{'maxElements' changed from 300 to 2000 in CNV Annotation tracks.}
	\item{NA 'border' no longer raise errors in draw.boxes() and draw.hist().}
	\item{'spacing' argument added to draw.boxes().}
	\item{'yaxs' argument added to draw.bg().}
	\item{'getChromEnd' virtual method added to the draw class, implemented by sliceable and track.lone.}
	\item{browsePlot() now accepts NA 'start' and 'end'.}
	\item{draw.bg() acuraccy problems in y axis resolved (workaround).}
	\item{tk.browse() acuraccy for zooming/unzooming sequences resolved (workaround).}
  }
}
\section{Changes in version 0.0.8 (2012-11-23)}{
  \itemize{
    \item{track.NCBI_genes() now also extracts the Gene database ID.}
    \item{'ylab' and 'ysub' added as drawing parameters, replacing 'trackName' and 'assembly'.}
    \item{getParameters() method added to drawable class, to be overloaded for class-specific defaults.}
  }
}
\section{Changes in version 0.0.7 (2012-11-08)}{
  \itemize{
    \item{read.gff3() added.}
    \item{browsePlot() now accepts '...' arguments, passed throught to the draw methods.}
    \item{'border' argument for draw.boxes() and draw.hist() now accepts 'color' to use filling color as border too.}
    \item{track.DGV_cnv() renammed in track.DGV1_cnv().}
    \item{track.DGV2_cnv() added for the beta version of DGV.}
  }
}
\section{Changes in version 0.0.6 (2012-10-29)}{
  \itemize{
    \item{refTable$indexes() error 'Name not found' know lists concerned names.}
    \item{refTable$rowOrder() know calls buildIndex() to keep it up-to-date.}
  }
}
\section{Changes in version 0.0.5 (2012-10-23)}{
  \itemize{
    \item{subtrack() no longer returns data.frames with arbitrary row.names lengths.}
    \item{subtrack() and lower level code now relies on an index rather than binary search for chromosome ranges.}
    \item{sliceable$cross() non-virtual method added.}
    \item{track.lone$isArmed() method added, now checked in splitArms and mergeArms.}
    \item{refTable methods setRowNames() and colNames() arguments renammed in 'value' for consistency.}
    \item{refTable methods setRowNames(), setColNames(), addEmptyRows(), addColumn() and fill() now discard value names, to produce proper data.frames.}
  }
}
\section{Changes in version 0.0.4 (2012-10-16)}{
  \itemize{
    \item{refTable$colOrder() method added.}
    \item{.chromosomes, .makeNames and .orderCols arguments added to track.lone constructor.}
  }
}
\section{Changes in version 0.0.3 (2012-10-12)}{
  \itemize{
    \item{track.lone$splitArms() and track.lone$mergeArms() methods added.}
  }
}
\section{Changes in version 0.0.2 (2012-10-09)}{
  \itemize{
    \item{'...' arguments were not passed from tk.files to tk.file.}
    \item{browsePlot() now accepts non-drawable objects in trackList, just ignore them.}
    \item{'Designs' added to tk.browse for better management of cghRA.probes objects.}
  }
}
\section{Changes in version 0.0.1 (2012-09-26)}{
  \itemize{
    \item{'blocking' argument added to tk.convert() and tk.browse(), for stand alone version.}
    \item{tk.convert() and tk.browse() are no longer embeddable.}
  }
}
