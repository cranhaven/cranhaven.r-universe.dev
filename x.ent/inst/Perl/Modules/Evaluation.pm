#crée par: PHAN Trong Tien
#date: 12/05/2014
#===========================================
package Modules::Evaluation;
$VERSION = v1.0;
use strict;
use warnings;
use utf8;
use open(IO => ':encoding(utf8)');
binmode(STDERR, ':utf8');
binmode(STDOUT, ':utf8');
binmode(STDIN, ':utf8');# All output will be UTF-8
our $CALCULE_RELATION = 0;
use Carp; #helps with debugging
#=========================================Debut module
my %data_eval = ();
my %data_result = ();
#load file evalation
#return a hashtable multimention, le format de hash: key=>nom_fichier:entité, value => des valeurs d'un entité
sub LoadFile
{
	my ($fileName) = @_;
	my %hash = ();#contenir tous les données
	my (@phases,@tokens);
	my $count_relation = 0; 
	open(EVAL,$fileName) || die("can't read this file: $fileName\n");
	while(<EVAL>)
	{
		#lower a string
		my $line = $_;
		#format de une ligne, pas relation: fichier:entité:$:valeur1:valeur2:valeur3
		#trim both ends
		#$line =~ s/^\s+|\s+$//g;
		#vérifier le linge est l'entité normée ou la rélation
		$line =~ s/’/'/g;
		my $num_usd = 0;
		
		$num_usd++ while($line =~ m/\$/g);	
		
		if($num_usd eq  1)#entité normée
		{
			@phases = split(/\$/,$line);
			if(@phases eq 2)
			{
				my @syns = ();
				my %temp = ();
				@tokens = split(/[\:]/,$phases[1]);
				for(my $i=1; $i<@tokens - 1; $i++)
				{
					#push(@syns,lc($tokens[$i]));#push(@syns,lc($tokens[$i]));
					$temp{lc($tokens[$i])}++;
				}
				foreach my $key (keys %temp)
				{
					push(@syns,$key)
				}
				$hash{lc($phases[0])} = \@syns;	
			}	
		}
		elsif($num_usd eq  2)
		{
			#relations
			#format de une ligne relation: fichier:entité1:entité2:valeur1:valeur2:1
			@phases = split(/\$\$/,$line);
			if(@phases eq 2)
			{
				my @syns = ();
				if(exists $hash{lc($phases[0])})
				{
					@syns = @{$hash{lc($phases[0])}};
				}
				$phases[1] =~ s/\r\n$//g;
				push(@syns,lc($phases[1]));
				$hash{lc($phases[0])} = \@syns;	
			}		
		}
=begin
		if($CALCULE_RELATION eq 1)
		{
			$line =~ s/\r\n$//g;
			if((@phases eq 1) and (length($line) > 1))
			{
				my @syns = ();
				push(@syns,lc($line));
				$hash{lc($line)}=\@syns;
				#$count_relation += 1;
			}
		}
=cut
	}
	close(EVAL);
	#print "Count relation: $count_relation\n";
	return %hash;
}
#author: PHAN Trong Tien
#date: 16/02/2015
#function: save results' relation
#format: relation,n_left,n_right,precision,recall,f-mesure
#input: evaluation file, extraction relation file
#output: a string has a format above
sub SaveEvaluationRelation
{
	my ($file_eval,$file_output,$n_left,$n_right) = @_;
	%data_eval = LoadFile($file_eval);
	%data_result = LoadFile($file_output);
	#relation
	my %countR_X = ();#eval
	my %countR_Y = ();#result
	my %countR_X_Y = ();#réunion entre la evaluation et le résultat
	my (@data_X,@data_Y);
	while(my ($key,$value)=each(%data_result))
	{
		@data_Y = @{$value};
		my @entites = split(/[\:]/,$key);
		if (scalar(@entites) ne 2) #rélations
		{
			#calculer pour chaque relation par exemple: p:s
			if($key =~ m/$entites[0]:(.*)\:/g)
			{
				$countR_Y{$1} += scalar(@data_Y);#result
				if(exists $data_eval{$key})
				{
					my %valuesR_X = ();
					my $val = ();
					@data_X =  @{$data_eval{$key}};
					foreach(@data_X)
					{
						$val = $_;
						$valuesR_X{$val}++;
					}
					$countR_X{$1} += Modules::Utils::SizeHash(%valuesR_X);;#eval
					foreach(@data_Y)
					{
						if(exists $valuesR_X{$_})
						{
							$countR_X_Y{$1}++;
						}	
					}
				}
			}	
		}
	}
	my $output = "";
	#précision et rappel relation
	my $count_relation_X = 0;#X est eval
	my $count_relation_Y = 0;#Y est result
	my $count_relation_X_Y = 0;
	foreach my $key (keys %countR_Y)
	{
		if(exists($countR_X{$key}))
		{
			$count_relation_X += $countR_X{$key};	
		}
		if(exists($countR_Y{$key}))
		{
			$count_relation_Y += $countR_Y{$key};
		}
		if(exists $countR_X_Y{$key})
		{
			$count_relation_X_Y += $countR_X_Y{$key};
			my $precision = $countR_X_Y{$key}*100/$countR_Y{$key};
			my $recall = $countR_X_Y{$key}*100/$countR_X{$key};
			my $f_mesure = (2*$recall*$precision)/($recall+$precision);
			#print "Relation $key: P = $precision\t-\tR = $recall \tCount: X_Y= $countR_X_Y{$key} -\tX= $countR_X{$key} -\tY = $countR_Y{$key} -\tF-mesure = $f_mesure\n";
			$output .= "$key,$n_left,$n_right,$precision,$recall,$f_mesure\n";
		}
	}
	my $precision = 0;
	my $recall = 0;
	my $f_mesure = 0;
	if(($count_relation_X > 0) and ($count_relation_Y > 0))
	{
		$precision = $count_relation_X_Y*100/$count_relation_Y;
		$recall = $count_relation_X_Y*100/$count_relation_X ;
		if(($recall+$precision) ne 0)
		{
			$f_mesure = (2*$recall*$precision)/($recall+$precision);
		}	
	}
	#print "Total of relation: P = $precision\t-\tR = $recall \tCount: X_Y= $count_relation_X_Y -\tX= $count_relation_X -\tY = $count_relation_Y -\tF-mesure = $f_mesure\n";
	$output .= "Total,$n_left,$n_right,$precision,$recall,$f_mesure\n";
	return $output;
} 
#cette function a deux arguments, primière: fichier eval.txt, deuxième: fichier output.txt
sub EvaluationGlobal
{
	my ($file_eval,$file_output) = @_;
	%data_eval = LoadFile($file_eval);
	%data_result = LoadFile($file_output);
	#Modules::Utils::PrintHashOfArray(%data_eval);
	my %countX = ();#eval
	my %countY = ();#result
	my %countX_Y = ();#réunion entre la evaluation et le résultat
	#relation
	my %countR_X = ();#eval
	my %countR_Y = ();#result
	my %countR_X_Y = ();#réunion entre la evaluation et le résultat
	my (@data_X,@data_Y);
	my $output = "Entity;Précision;Rappel;X_Y;X;Y;F-mesure\n";
	while(my ($key,$value)=each(%data_result))
	{
		@data_Y = @{$value};
		#compter suivi chaque entité: p, m,b
		my @entites = split(/[\:]/,$key);
		if (scalar(@entites) eq 2)#seulement des entité normée, pas rélations
		{
			$countY{$entites[1]} += scalar(@data_Y);
			if(exists $data_eval{$key})
			{
				my %values_X = ();
				my $val = ();
				@data_X =  @{$data_eval{$key}};
				foreach(@data_X)
				{
					$val = $_;
					$values_X{$val}++;
				}
				$countX{$entites[1]} += Modules::Utils::SizeHash(%values_X);
				foreach(@data_Y)
				{
					if(exists $values_X{$_})
					{
						$countX_Y{$entites[1]}++;
					}	
				}	
			}
		}
		else #relations
		{
			#calculer pour chaque relation par exemple: p:s
			if($key =~ m/$entites[0]:(.*)\:/g)
			{
				$countR_Y{$1} += scalar(@data_Y);#result
				if(exists $data_eval{$key})
				{
					my %valuesR_X = ();
					my $val = ();
					@data_X =  @{$data_eval{$key}};
					foreach(@data_X)
					{
						$val = $_;
						$valuesR_X{$val}++;
					}
					$countR_X{$1} += Modules::Utils::SizeHash(%valuesR_X);;#eval
					foreach(@data_Y)
					{
						if(exists $valuesR_X{$_})
						{
							$countR_X_Y{$1}++;
						}	
					}
				}
			}
		}
	}
	#précision et rappel entité normée
	my $count_global_X = 0;
	my $count_global_Y = 0;
	my $count_global_X_Y = 0;
	foreach my $key (keys %countY)
	{
		if(exists($countX{$key}))
		{
			$count_global_X += $countX{$key};
		}
		if(exists($countY{$key}))
		{
			$count_global_Y += $countY{$key};
		}
		if(exists $countX_Y{$key})
		{
			$count_global_X_Y += $countX_Y{$key};
			my $precision = $countX_Y{$key}*100/$countY{$key};
			my $recall = $countX_Y{$key}*100/$countX{$key};
			my $f_mesure = (2*$recall*$precision)/($recall+$precision);
			print "Entity $key: P = $precision\t-\tR = $recall \tCount: X_Y= $countX_Y{$key} -\tX= $countX{$key} -\tY = $countY{$key} -\tF-mesure = $f_mesure\n";
			$output .= "$key;$precision;$recall;$countX_Y{$key};$countX{$key};$countY{$key};$f_mesure\n";
		}
	}
	my $precision = 0;
	my $recall = 0;
	my $f_mesure = 0;
	if(($count_global_Y > 0) and ($count_global_X > 0))
	{
		$precision = $count_global_X_Y*100/$count_global_Y;
		$recall = $count_global_X_Y*100/$count_global_X ;
		$f_mesure = (2*$recall*$precision)/($recall+$precision);	
	}
	print "Total: P = $precision\t-\tR = $recall \tCount: X_Y= $count_global_X_Y -\tX= $count_global_X -\tY = $count_global_Y -\tF-mesure = $f_mesure\n";
	$output .= "Total;$precision;$recall;$count_global_X_Y;$count_global_X;$count_global_Y;$f_mesure\n";
	print "----------------------------------------------------------------------------------------------------------------------\n";
	$output .= "Relation\n";
	#précision et rappel relation
	my $count_relation_X = 0;#X est eval
	my $count_relation_Y = 0;#Y est result
	my $count_relation_X_Y = 0;
	foreach my $key (keys %countR_Y)
	{
		if(exists($countR_X{$key}))
		{
			$count_relation_X += $countR_X{$key};	
		}
		if(exists($countR_Y{$key}))
		{
			$count_relation_Y += $countR_Y{$key};
		}
		if(exists $countR_X_Y{$key})
		{
			$count_relation_X_Y += $countR_X_Y{$key};
			my $precision = $countR_X_Y{$key}*100/$countR_Y{$key};
			my $recall = $countR_X_Y{$key}*100/$countR_X{$key};
			my $f_mesure = (2*$recall*$precision)/($recall+$precision);
			print "Relation $key: P = $precision\t-\tR = $recall \tCount: X_Y= $countR_X_Y{$key} -\tX= $countR_X{$key} -\tY = $countR_Y{$key} -\tF-mesure = $f_mesure\n";
			$output .= "$key;$precision;$recall;$countR_X_Y{$key};$countR_X{$key};$countR_Y{$key};$f_mesure\n";
		}
	}
	$precision = 0;
	$recall = 0;
	$f_mesure = 0;
	if(($count_relation_X > 0) and ($count_relation_Y > 0))
	{
		$precision = $count_relation_X_Y*100/$count_relation_Y;
		$recall = $count_relation_X_Y*100/$count_relation_X ;
		if(($recall+$precision) ne 0)
		{
			$f_mesure = (2*$recall*$precision)/($recall+$precision);
		}	
	}
	print "Total of relation: P = $precision\t-\tR = $recall \tCount: X_Y= $count_relation_X_Y -\tX= $count_relation_X -\tY = $count_relation_Y -\tF-mesure = $f_mesure\n";
	$output .= "Total of relation;$precision;$recall;$count_relation_X_Y;$count_relation_X;$count_relation_Y;$f_mesure\n";
	#Modules::Utils::SaveFile("./data/out/result.csv",$output);
}
sub EvaluationLocal
{
	my ($file_eval,$file_output) = @_;
	%data_eval = LoadFile($file_eval);
	%data_result = LoadFile($file_output);
	#il y a des prblème avec des cas: - pluriels et singulier, accent et pas accent
	#X: eval
	#Y: result
	while(my ($key,$value)=each(%data_result))
	{
		print  "Precision et Rapple => $key \n";
		my $count_sub_X_Y = 0;
		my (@data_X,@data_Y);
		@data_Y = @$value;#en stokant valeurs de entité test
		my ($entiteX,$entiteY,$entiteX_Y) = ();
		if(exists $data_eval{$key})
		{
			@data_X =  @{$data_eval{$key}};
			my %countX = ();
			my %countY = ();
			my %countX_Y = ();
			my $val = ();
			$entiteX = "Eval=>"; 
			foreach(@data_X)
			{
				$val = $_;
				$countX{$val}++;
			}
			$entiteY = "\nResultat=>";
			$entiteX_Y = "\nEval=Resultat=>";
			foreach(@data_Y)
			{
				$countY{$_}++;
				if(exists $countX{$_})
				{
					$countX_Y{$_}++;
					$count_sub_X_Y++;
				}	
			}
			#imprimer les résultats
			for my $key (keys %countX_Y)
			{
				delete $countX{$key};
				delete $countY{$key};
				$entiteX_Y .= "$key:";
			}
			for my $key (keys %countX)
			{
				$entiteX .= $key.":";
			}
			for my $key (keys %countY)
			{
				$entiteY .= "$key:";
			}
			print $entiteX;
			print $entiteY;
			print $entiteX_Y;	
			print "\n";
		}
		if((@data_X >0) and (@data_Y > 0))
		{
			my $presition = $count_sub_X_Y*100/@data_Y;
			my $recall = $count_sub_X_Y*100/@data_X;
			print "P = $presition\t-\tR = $recall \tCount: X_Y= $count_sub_X_Y -\tX =".scalar(@data_X)."-\tY= ".scalar(@data_Y)."\n";
		
		}
	}
}
#=========================================Fin module
1;