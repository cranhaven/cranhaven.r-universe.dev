use strict;
use warnings;
binmode(STDOUT, ':raw:encoding(UTF-8)');
my $DIR_INPUT = "/Volumes/MacOS/Users/phantrongtien/Desktop/Source";
my $DIR_OUTPUT = "/Volumes/MacOS/Users/phantrongtien/Desktop/Cible";
print "Commencer\n";
foreach my $fp (glob("$DIR_INPUT/*.{txt,xml}"))
{
	my $f_name = ( split m{/}, $fp )[-1];
   	open(INFILE, "<:raw:encoding(UTF-16)", $fp) or die("Can't open \"$fp\": $!\n");
	open(OUTFILE, '>:encoding(UTF-8)', $DIR_OUTPUT."/$f_name") or die $!;
	while (<INFILE>) {
   		print OUTFILE $_;
	}
	close INFILE;
	close OUTFILE;
}
print "Fini";