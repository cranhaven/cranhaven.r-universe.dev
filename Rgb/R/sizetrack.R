# Memory and time efficient subsetting of chromosomal tracks (chrom, start, end) (count hits only)
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

sizetrack = function(...) {
	.External("track", PACKAGE="Rgb", mode="size", ...)
}
