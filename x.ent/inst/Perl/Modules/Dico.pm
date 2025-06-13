#crée par: PHAN Trong Tien
#date: 12/05/2014
#===========================================
package Modules::Dico;
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
#get data from a file input, store names et synonymes into a hash table
#using the file: maladies, pathogenes, plantes, stades
sub LoadDico
{
	#get a file name from argument
	my ($fileName,$node,$col_key,$col_val) = @_;
	#le format colone valeur: 1..* ou 1,3,5 ou 3..3,5..* ou 1,3,5..*
	my @col_vals = Modules::Utils::CheckColumn($col_val);
	######################
	my %hash = ();
	my %temp = ();#stocker les lignes en ayant le mot N
	my @phases;
	$col_key = $col_key - 1;
	open(DICO,'<:raw:encoding(UTF8)',$fileName) || die("can't read this file: $fileName\n");
	while(<DICO>)
	{
		#lower a string
		my $line = $_;
		#trim both ends
		$line =~ s/^\s+|\s+$//g;
		$line =~ s/’/'/g;
		@phases = split(/[\t:;]/,$line);
		my ($label,$value) = ();
		if($node eq "true" )
		{
			if(scalar(@phases) > 1)
			{
				#si c'est une feuille
				if($phases[1] ne "N")
				{
					my @syns = ();
					my $label = $phases[$col_key];
					$label =~ s/^\s+|\s+$//g;
					push(@syns,$label);
					my $max = 0; 
					foreach(@col_vals)
					{
						if($_ =~ /^[0-9]+$/)
						{
							my $id  = $_ - 1;
							if($id < scalar(@phases))
							{
								$max = $id;
								$value = $phases[$id];
								$value =~ s/^\s+|\s+$//g;
								if($phases[$id] ne "")
								{
									push(@syns,$value);		
								}
							}
						}else
						{
							for(my $i=$max+1; $i<@phases; $i++)
							{
								$value = $phases[$i];
								$value =~ s/^\s+|\s+$//g;
								if($phases[$i] ne "")
								{
									push(@syns,$value);		
								}
							}
						}
						
					}
					$hash{lc($label)} = \@syns;
				}
				#si c'est une racine
				if($phases[1] eq "N")
				{
					my @syns = ();
					$label = $phases[$col_key];
					$label =~ s/^\s+|\s+$//g;
					push(@syns,$label);
					my $max = 0; 
					foreach(@col_vals)
					{
						if($_ =~ /^[0-9]+$/)
						{
							my $id = $_ - 1;
							if($id < scalar(@phases))
							{
								$max = $id;
								$value = $phases[$id];
								$value =~ s/^\s+|\s+$//g;
								if($value ne "")
								{
									push(@syns,$phases[$id]);
								}	
							}
						}
						else
							{
								for(my $i=$max+1; $i<@phases; $i++)
								{
									$value = $phases[$i];
									$value =~ s/^\s+|\s+$//g;
									if($value ne "")
									{
										push(@syns,$value);
									}
								}
							}
					}
					$temp{lc($label)} = \@syns;
				}
			}	
		}else#pas node
		{
			my @syns = ();
			#télechargement le code postal
			my $max = 0;
			$label = $phases[$col_key];
			$label =~ s/^\s+|\s+$//g;
			push(@syns,$label); 
			foreach(@col_vals)
			{
				if($_ =~ /^[0-9]+$/)
				{
					my $id  = $_ - 1;
					if($id < scalar(@phases))
					{
						$max = $id;
						$value = $phases[$id];
						$value =~ s/^\s+|\s+$//g;
						if($phases[$id] ne "")
						{
							push(@syns,$value);
						}	
					}
				}else
				{
					for(my $i=$max+1; $i<@phases; $i++)
					{
						$value = $phases[$i];
						$value =~ s/^\s+|\s+$//g;
						if($phases[$i] ne "")
						{
							push(@syns,$value);
						}
					}
				}
				
			}
			$hash{$label} = \@syns;
		}
	}
	#vérifier %temp, si un mot a déjà existé dans %hash, on vais déplacer ce mot
	if($node eq "true")
	{
		foreach my $key (keys %temp)
		{
			my @arr = @{$temp{$key}};
			my @temp_array = ();
			foreach(@arr)
			{
				my $test = $_; 
				if(exists $hash{lc($test)})
				{
					push(@temp_array,$test);
				}
			}
			foreach(@temp_array)
			{
				#enlever de l'array
				my $test = $_;
				@arr = grep {!/^$test/i} @arr;	
			}
			#ajouter à hash
			#Modules::Utils::PrintArray(@arr);
			$hash{$key} = \@arr;
		}	
	}
	#Modules::Utils::PrintHashOfArray(%hash);
	close(DICO);
	undef %temp;
	return %hash;	 
}
sub LoadDicoNameAndSyn
{
	#format fichier dico:
	#il y a deux types données: 
	#entité_org:N:entité_syn1, entité_syn2........
	#entité_org:L:entité_syn2, entité_syn2........
	#get a file name from argument
	my ($fileName) = @_;
	my %hash = ();
	my @phases;
	open(DICO,'<:raw:encoding(UTF8)',$fileName) || die("can't read this file: $fileName\n");
	while(<DICO>)
	{
		#lower a string
		my $line = $_;
		#trim both ends
		$line =~ s/^\s+|\s+$//g;
		@phases = split(/:/,$line);
		my @syns = ();
		#print $line."\t".@phases."\n";
		#{
		for(my $i=0; $i<@phases; $i++)
		{
			push(@syns,$phases[$i]);
		}
		$hash{$phases[0]} = \@syns;
	}
	close(DICO);
	return %hash;
}
sub LoadDicoCSV
{
	my ($fileName) = @_;
	my %hash = ();
	my @phases;
	open(DICO,'<:raw:encoding(UTF8)',$fileName) || die("can't read this file: $fileName\n");
	while(<DICO>)
	{
		#lower a string
		my $line = $_;
		#trim both ends
		$line =~ s/^\s+|\s+$//g;
		@phases = split(/;/,$line);
		$hash{lc($phases[1])} = $phases[0];
	}
	close(DICO);
	return %hash;
}
#=========================================Fini module
1;