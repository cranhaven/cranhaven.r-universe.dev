use strict;
use warnings;
#Format data input, output UTF-8
use utf8;
use open(IO => ':encoding(utf8)');
binmode(STDERR, ':utf8');
binmode(STDOUT, ':utf8');
binmode(STDIN, ':utf8');# All output will be UTF-8

my $DIR_INPUT = "./data";
my $FILE_EVAL = "./data/eval/eval_utf8.txt";

my $FILE_DICO = "./dico/v3/dico-b_v3.txt";
my $TAG = "b";
#========================================
my %mots_predicts = ();
my %evals = LoadEval($FILE_EVAL);
my $type_dic_check;
my @data = (); 
foreach my $fp (glob("$DIR_INPUT/*.txt"))
{	
	printf "%s\n", $fp;
	#lay ten file
	my $f_name = ( split m{/}, $fp )[-1];
	$f_name =~ s{\.[^.]+$}{};
	$type_dic_check = $f_name.":$TAG:";#please change here
	if(exists $evals{$type_dic_check})
	{
		@data = @{$evals{$type_dic_check}};
	}
	foreach(@data)
	{
		$_ =~ s/^\s+|\s+$//gi;
		if(length($_)>0)
		{
			$mots_predicts{lc($_)}++;
		}
	}
}
CheckDico($FILE_DICO,%mots_predicts);#please change here
#=========================================================================
sub CheckDico
{
	my($fn,%mots) = @_;
	my @unwanted;
	open(DIC,'<:raw:encoding(UTF8)',$fn) || die("can't read this file: $fn\n");
	my $data = "";
	while(<DIC>)
	{
		$data .= $_;
		#print $line."\n";
		
	}
	foreach my $key (keys %mots)
	{
		if( $data =~ /[\r\n:]($key)[:]/gi) {
			push @unwanted, lc($1);
		}
		if( $data =~/^($key)[:]/gi) {
			push @unwanted, lc($1);
		}
	}
	close DIC;
	foreach(@unwanted)
	{
		delete $mots{$_};
	}
	print "---------------------------\n";
	#output
	#open(DIC,">>$fn") || die("can't read this file: $fn\n");
	#foreach my $key (keys %mots)
	#{
	#	print DIC $key.":\n";
	#}
	#close DIC;
	print "________________________________\n";
	foreach my $key (keys %mots)
	{
		print $key."\n";
		#print $key.":".uc($key).":\n";
	}
}
sub LoadEval
{
	my ($fileName) = @_;
	my %hash = ();
	my (@phases,@tokens);
	open(EVAL,'<:raw:encoding(UTF8)',$fileName) || die("can't read this file: $fileName\n");
	while(<EVAL>)
	{
		#lower a string
		my $line = $_;
		#trim both ends
		#$line =~ s/^\s+|\s+$//g;
		@phases = split(/[\$]/,$line);
		if(@phases eq 2)
		{
			my @syns = ();
			@tokens = split(/[\:]/,$phases[1]);
			for(my $i=1; $i<@tokens; $i++)
			{
				push(@syns,$tokens[$i]);
			}
			$hash{$phases[0]} = \@syns;	
		}
	}
	close(EVAL);
	return %hash;
}