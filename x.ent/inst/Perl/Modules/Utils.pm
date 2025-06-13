#crée par: PHAN Trong Tien
#date: 12/05/2014
#===========================================
package Modules::Utils;
$VERSION = v1.0;
use strict;
use warnings;
use Carp; #helps with debugging
use utf8;
use open(IO => ':encoding(utf8)');
binmode(STDERR, ':utf8');
binmode(STDOUT, ':utf8');
binmode(STDIN, ':utf8');# All output will be UTF-8
#=========================================Debut module
my $fin = "</fin>";
sub PrintHash
{
	my %hash = @_;
	foreach my $key (keys %hash)
	{
		my $value = $hash{$key};
		print "$key:$value\n";
	}
}
sub PrintArray
{
	my @array = @_;
	foreach(@array)
	{
		print $_."\n\n";
	}
}
sub CheckDate
{
	my ($year,$month,$day) = @_;
	my $year_sys = (localtime())[5] + 1900;
	#check year
	if(($year < 1900) || ($year > $year_sys))
	{
		$year = 1900;
	}	
	#check month
	if($month < 1 || $month > 12)
	{
		$month = '01';
	}
	#check day
	my @mdays = (0,31,28,31,30,31,30,31,31,30,31,30,31);
	if ($month == 2) {
    	 if ($year % 4 != 0) { $mdays[2] = 28; }
    	 elsif ($year % 400 == 0) { $mdays[2] = 29; }
         elsif ($year % 100 == 0) { $mdays[2] = 28; }
         else { $mdays[2] = 29; }
    }
    if($day < 1 || $day > $mdays[$month])
    {
    	$day = '01';
    }
    return ($year,$month,$day);
}
sub PrintHashOfArray
{
	my %hash = @_;
	foreach my $key (keys %hash)
	{
		print "$key:";
		my $values = $hash{$key};
		foreach my $nam ( @{$values} ) 
		{
			print  $nam."\t";
		}
	print  "\n";
	}	
}
#vérifier la taile de hash 
sub SizeHash
{
	my (%hash) = @_;
	my @keys = keys %hash;
	my $size = @keys;
	return $size;
}
#tranformer un hash devenu une chaine que chaque key sépare par deux points ":"
sub HashToString
{
	my %data = @_;
	my $temp = "";
	foreach my $key (keys %data)
	{
		$key =~ s/[+*?!]//gm;
		$key = Trim($key);
		if(length($key)>0)
		{
			$temp = $temp.$key.":";
		}
	}
	return $temp;
}
sub HashToArray
{
	my %data = @_;
	my @result = ();
	foreach my $key (keys %data)
	{
		#check empty
		if(length($key)>0)
		{
			push(@result,$key);	
		}
	}
	return @result;
}
#normaliser les données
sub Normalize
{
    my($str) = @_;
    $str =~ s/\n//gm;
    $str =~ s/\s\s+/ /gm;
    return $str;
}
#function readfile, return une variable type SCALAR
sub ReadFile
{
	my($fn) = @_;
	my $data = "";
	my $line = "";
	my $prev_line = "";
	open(INPUT,'<:raw:encoding(UTF8)',$fn) || die "can't read this file: $fn\n";
	while(<INPUT>)
	{
		$line = $_;
		$line =~ s/\s\s+/ /gm;
		#delete special characters
		$line =~ s/^[\-•]//g;
		$line = Trim($line);
		#if($line =~ /^(janvier|jan|février|fevrier|fev|mars|mar|MARS|avril|avr|mai|juin|jun|juillet|jul|août|AOÛT|aoû|septembre|sep|octobre|oct|novembre|nov|décembre|déc)([\\\/\[\]\{\},?;:.!\(\)\" \t]+)/g)
		#{
		#	print("WXXXXXXXXXXXXXXXXXXXXXCCCCCCCCCCCCCCCCCCCCCC\n");
		#	print $line."\n";
		#}
		#first paragraph
		if($line =~ /^[A-ZÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝ]/)
		{
			if(not ($line =~ /^(janvier|jan|février|fevrier|fev|mars|mar|MARS|avril|avr|mai|juin|jun|juillet|jul|août|AOÛT|aoû|septembre|sep|octobre|oct|novembre|nov|décembre|déc)/gi) || not ($prev_line =~ /\d$/g))
			{
				$line = "{S}".$line;
			}
		}
		#}
		if(length($line) > 0)
		{
			if((length($prev_line) eq 0))
			{
				if($data eq "")
				{
					$data .= $line;
				}
				else
				{
					if(not $data =~ /[\.\,\?\!\:]$/g)
					{
						$data .= ".".$line; #le cas contien la caractère spécial: centre de la région • rouille  jaune
					}
					else
					{
						$data .= " ".$line;
					}	
				}		
			}
			else # voilà saute de linge, on vais combiner avec une phrase précédent
			{
				#vérifier si debut une phase est une caractère majuscule => ne pas combiner avec la phase précédente
				if($data eq  "")
				{
					$data = $line;
				}
				else
				{
					if(not $prev_line =~ /[\.\,\?\!\:]$/g)
					{
						if($line =~/^{S}/g)#if begining is a character upcase 
						{
							if($prev_line =~ /\d$/g)#check number
							{
								$data .= " ".$line;
							}
							else
							{
								$data .= ".".$line; #le cas contien la caractère spécial: centre de la région • rouille  jaune	
							}
						}
						else
						{
							$data .= " ".$line;
						}
					}
					else
					{
						$data .= $line;#if end of sentence is a puntuation
					}	
				}	
			}
			$prev_line = $line;
		}
	}
	#modifier les mots avec un tiret
	#$data =~ s/([Saint|saint|SAINT|St])\- ([A-Za-zÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïñòóôõöùúûüýÿ]+)/$1-$2/g;#Saint- XXXX
	#$data =~ s/(\b[A-Za-zÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝ]+\b)\-(\b[A-Za-zÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝ]+\b)/$1$2/g;
	$data =~ s/([A-Za-zÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïñòóôõöùúûüýÿ]+)\- ([a-zàáâãäåçèéêëìíîïñòóôõöùúûüýÿ]+)/$1$2/g;#supprimer la caractère est - si un mot a été coupé 
	$data =~ s/([A-Za-zÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïñòóôõöùúûüýÿ]+)\-([A-ZÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝ]+)/$1-$2/g;
	$data =~ s/([A-Za-zÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïñòóôõöùúûüýÿ]+)\-([0-9]+)/$1 - $2/g;#Party-21
	$data =~ s/œ/oe/gi;
	$data =~ s/’/'/g;
	$data =~ s/ :|: |::/:/gm;
	if(not $data =~ /$fin$/g)
	{
		$data .= "$fin";
	}
	#$data =~ s/\s\s+/ /gm;
	close(INPUT);
	return $data;
}
#la function a deux paramètres: nom de fichier sortir, fichier + entité, hash contient des valeur
sub SaveFile
{
	my ($file_output,$data) = @_;
	open(OUTPUT,'>:raw:encoding(UTF8)',$file_output) || die "Can't open this file: $file_output";
	print OUTPUT $data."\n";
	close(OUTPUT);
}
sub ReplaceWord
{
	my ($data, $file) = @_;
	my %mots_alternative = Modules::Dico::LoadDicoNameAndSyn($file);
	#replate le caractère ’ -> '
    for my $key (keys %mots_alternative)
	{
		my @elements = @{$mots_alternative{$key}};
		foreach(@elements)
		{
			$data =~ s/([\\\[\]\{\}.,?;:!\(\)\" \t]+)($_)([\\\[\]\{\}.,?;:!\(\)\" \t]+)/$1$key$3/g;	
		} 
	}	
	return $data
}
# Declare the trim subroutine
sub Trim
{
  my $string = shift;
  $string =~ s/^\s+//;            
  $string =~ s/\s+$//; 
  return $string;         
}
# vérifier le character primier est upper
sub CheckUpCase {
    if ($_[0] =~ /^[A-ZÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝ]/) {
        return 1;
    }
    else {
        return 0;
    }
}
#le fonction utilise à fichier config, vérifier des colonnes et retourner le nummer de collone 
sub CheckColumn
{
	my ($col_val) = @_;
	my @result = ();
	if(length($col_val) > 0)
	{
		my @arr = split(/[\,]/,$col_val);
		foreach(@arr)
		{
			#si seleument number
			if($_ =~ /^[0-9]+$/)
			{
				push @result,$_;
			}
			else
			{
				my @arr1 = split(/[\..]/,$_);
				if(scalar(@arr1) eq 3)
				{
					if(($arr1[0] =~ /^[0-9]+$/) and ($arr1[2] =~ /^[0-9]+$/))
					{
						if($arr1[0] > $arr1[2])
						{
							my $tg = $arr1[0];
							$arr1[0] = $arr1[2];
							$arr1[2] = $tg;
						}
						for(my $i = $arr1[0];$i <= $arr1[2];$i++)
						{
							push @result,$i;
						}
					}
					else
					{
						push @result,$arr1[0];
						push @result,$arr1[2]; 
					}
				}
			}
		}	
	}
	return @result;
}
#=========================================Fin module
1;