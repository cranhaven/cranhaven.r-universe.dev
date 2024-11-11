# Memory and time efficient subsetting of chromosomal tracks (chrom, start, end)
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

subtrack = function(...) {
	.External("track", PACKAGE="Rgb", mode="sub", ...)
}
