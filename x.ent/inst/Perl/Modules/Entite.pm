#crée par: PHAN Trong Tien
#date: 12/05/2014
#===========================================
package Modules::Entite;
$VERSION = v1.0;
use strict;
use warnings;
use utf8;
use open(IO => ':encoding(utf8)');
binmode(STDERR, ':utf8');
binmode(STDOUT, ':utf8');
binmode(STDIN, ':utf8');# All output will be UTF-8
use Carp; #helps with debugging
#=========================================Debut module
our @oldPhases;#stoker les paragraphes qui ne contenont pas des balises
my $N_GRAM = 4; 
#Appliquer le règles de date
#un paramètre pour la fonction
sub ApplyRuleDate
{
	my %months = ("01" => "01","1" => "01","janvier" => "01","jan"=>"01",
	"02" => "02","2" => "02","février"=>"02","fevrier"=>"02","fév"=>"02","fev"=>"02",
	"03" => "03","3" => "03","mars"=>"03","mar"=>"03",
	"04" => "04","4" => "04","avril"=>"04","avr"=>"04",
	"05" => "05","5" => "05","mai"=>"05",
	"06" => "06","6" => "06","juin"=>"06","jun"=>"06",
	"07" => "07","7" => "07","juillet"=>"07","jul"=>"07",
	"08" => "08","8"=>"08","août" => "08","aout" => "08","aoû"=>"08","aou"=>"08","aoÛt"=>"08",
	"09" => "09","9"=>"09","septembre" => "09","sep"=>"09",
	"10" => "10","octobre" => "10","oct"=>"10",
	"11" => "11","novembre" => "11","nov"=>"11",
	"12" => "12","décembre" => "12","déc"=>"12","decembre" => "12","dec"=>"12"); 
	my ($line,$exp) = @_;
	my $first = 0;
	# date precise 05-05-2011 or 05/05/2011 or 05/mai/2011, ect
	if(($line =~ /(\d{1,2})[\-\/\.\s](0[1-9]|1[012]|janvier|jan|février|fevrier|fev|mars|mar|avril|avr|mai|juin|jun|juillet|jul|août|aoû|septembre|sep|octobre|oct|novembre|nov|décembre|déc)[\-\/\.\s](\d{4})/gi) and ($first == 0))
	{
		my $date;
		if(length($1) eq 1)
		{
			$date = "0".$1.".".$months{lc($2)}.".".$3;
		}
		else
		{
			$date = $1.".".$months{lc($2)}.".".$3;
		}
		$line =~ s/(\d{1,2})[\-\/\.\s](0[1-9]|1[012]|janvier|jan|février|fevrier|fev|mars|mar|avril|avr|mai|juin|jun|juillet|jul|août|aoû|septembre|sep|octobre|oct|novembre|nov|décembre|decembre|déc)[\-\/\.\s](\d{4})/<$exp>$date<\/$exp> $1\/$2\/$3/gi;
		$first = 1; 		
	}
	#date precise 05-2011 or 05/2011 or mai/2011
	if(($line =~ /(0[1-9]|1[012]|janvier|jan|février|fevrier|fev|mars|mar|MARS|avril|avr|mai|juin|jun|juillet|jul|août|AOÛT|aoû|septembre|sep|octobre|oct|novembre|nov|décembre|déc)[\-\/\.\s](\d{4})/gi) and ($first == 0))
	{
		my $date;
		$date = $months{lc($1)}.".".$2;
		$line =~ s/(0[1-9]|1[012]|janvier|jan|février|fevrier|fev|mars|mar|MARS|avril|avr|mai|juin|jun|juillet|jul|août|AOÛT|aoû|septembre|sep|octobre|oct|novembre|nov|décembre|déc)[\-\/\.\s](\d{4})/<$exp>$date<\/$exp> $1\/$2/gi;
		$first = 1; 		
	}
	
	return $line;
}
#using for validation of date
sub GetOneDate
{
	my %months = ("01" => "01","1" => "01","janvier" => "01","jan"=>"01",
	"02" => "02","2" => "02","février"=>"02","fevrier"=>"02","fév"=>"02","fev"=>"02",
	"03" => "03","3" => "03","mars"=>"03","mar"=>"03",
	"04" => "04","4" => "04","avril"=>"04","avr"=>"04",
	"05" => "05","5" => "05","mai"=>"05",
	"06" => "06","6" => "06","juin"=>"06","jun"=>"06",
	"07" => "07","7" => "07","juillet"=>"07","jul"=>"07","juil"=>"07",
	"08" => "08","8"=>"08","août" => "08","aout" => "08","aoû"=>"08","aou"=>"08","aoÛt"=>"08",
	"09" => "09","9"=>"09","septembre" => "09","sep"=>"09",
	"10" => "10","octobre" => "10","oct"=>"10",
	"11" => "11","novembre" => "11","nov"=>"11",
	"12" => "12","décembre" => "12","déc"=>"12","decembre" => "12","dec"=>"12"); 
	my ($line) = @_;
	my $year = "1900";
	my $month = "01";
	my $day = "01";
	#get current date for comparing date of extraction
	my ($sec,$min,$hour,$mday_sys,$mon_sys,$year_sys,$wday,$yday,$isdst) =  localtime(time);
	$year_sys %= 100; #example: 2015 => $year_sys = 115
	#if date is YYYY-MM-DD
	if(($line =~ /(\d{4})[\_\-\/\.\s](0[1-9]|1[012]|janvier|jan|février|fevrier|fev|mars|mar|avril|avr|mai|juin|jun|juillet|jul|juil|août|aoû|septembre|sep|octobre|oct|novembre|nov|décembre|déc)[\_\-\/\.\s](\d{1,2})/gi) || ($line =~ /(\d{4})(0[1-9]|1[012]|janvier|jan|février|fevrier|fev|mars|mar|avril|avr|mai|juin|jun|juillet|juil|jul|août|aoû|septembre|sep|octobre|oct|novembre|nov|décembre|déc)(\d{1,2})/gi))
	{ 
		if(length($3) eq 1)
		{
			$day = "0".$3;
		}
		else
		{
			$day = $3;
		}
		$year = $1;
		if(($year >= 1946) && ($year <= $year_sys+2000))
		{
			#print "test1";
			$month = $months{lc($2)};
			($year,$month,$day) = Modules::Utils::CheckDate($year,$month,$day);
			return ($year."-".$month."-".$day,$day."-".$month."-".$year);	
		}
	}
	# date  05-05-2011 or 05/05/2011 or 05/mai/2011, ect
	if(($line =~ /(\d{1,2})[\_\-\/\.\s](0[1-9]|1[012]|janvier|jan|février|fevrier|fev|mars|mar|avril|avr|mai|juin|jun|juillet|jul|juil|août|aoû|septembre|sep|octobre|oct|novembre|nov|décembre|déc)[\_\-\/\.\s](\d{4})/gi) || ($line =~ /(\d{1,2})(janvier|jan|février|fevrier|fev|mars|mar|avril|avr|mai|juin|jun|juillet|jul|juil|août|aoû|septembre|sep|octobre|oct|novembre|nov|décembre|déc)(\d{4})/gi))
	{
		#check day, default DDMMYY(YY)
		$day = $1;
		$year = $3;
		
		if(length($day) eq 1)
		{
			$day = "0".$day;
		}
		
		#YYYY-MM-DD
		if(($year >= 1946) && ($year <= $year_sys+2000))
		{
			$month = $months{lc($2)};
			($year,$month,$day) = Modules::Utils::CheckDate($year,$month,$day);
			#print "test2";
			return ($year."-".$month."-".$day,$day.".".$month.".".$year);		
		}
	}
	# date  05-05-11 or 05/05/11 or 05/mai/11, ect
	if(($line =~ /(\d{1,2})[\_\-\/\.\s](0[1-9]|1[012]|janvier|jan|février|fevrier|fev|mars|mar|avril|avr|mai|juin|jun|juillet|jul|juil|août|aoû|septembre|sep|octobre|oct|novembre|nov|décembre|déc)[\_\-\/\.\s](\d{2,4})/gi) || ($line =~ /(\d{1,2})(janvier|jan|février|fevrier|fev|mars|mar|avril|avr|mai|juin|jun|juillet|jul|juil|août|aoû|septembre|sep|octobre|oct|novembre|nov|décembre|déc)(\d{2,4})/gi))
	{
		#check day, default DDMMYY perhaps YYMMDD
		$day = $1;
		$year = $3;
		if(($day < 1) || ($day > 31))
		{
			$day = $3;#if format is YYMMDD
			$year = $1;
		}
		if(length($day) eq 1)
		{
			$day = "0".$day;
		}
		#check year
		if(length($year) eq 2)
		{
			#case: year is 09, 10,etc => 2009,2010, versus 45, 46 => 1945, 1946
			if($year < $year_sys)
			{
				$year = 2000 + $year;
			}
			else
			{
				$year = 1900 + $year;
			}
		}
		#YYYY-MM-DD
		if(($year >= 1946) && ($year <= $year_sys+2000))
		{
			$month = $months{lc($2)};
			($year,$month,$day) = Modules::Utils::CheckDate($year,$month,$day);
			return ($year."-".$month."-".$day,$day.".".$month.".".$year);	
		}
	}
	#date precise 05-2011 or 05/2011 or mai/2011
	if($line =~ /(0[1-9]|1[012]|janvier|jan|février|fevrier|fev|mars|mar|MARS|avril|avr|mai|juin|jun|juillet|jul|juil|août|AOÛT|aoû|septembre|sep|octobre|oct|novembre|nov|décembre|déc)[\-\/\.\s](\d{2,4})/gi)
	{
		#check year
		if(length($2) eq 2)
		{
			#case: year is 09, 10,etc => 2009,2010, versus 45, 46 => 1945, 1946
			if($2 < $year_sys)
			{
				$year = 2000 + $2;
			}
			else
			{
				$year = 1900 + $2;
			}
		}
		else
		{
			$year = $2;
		}
		if(($year >= 1946) && ($year <= $year_sys+2000))
		{
			$month = $months{lc($1)};
			($year,$month,$day) = Modules::Utils::CheckDate($year,$month,$day);
			#print "test4";
			return ($year."-".$month."-".$day,$day.".".$month.".".$year);			
		}
	}
	#only year
	if($line =~ /[\_\-\/\.\s](\d{4})[\_\-\/\.\s]/gi)
	{
		$year = $1;
		if(($year >= 1946) && ($year <= $year_sys+2000))
		{
			#print "test5";
			($year,$month,$day) = Modules::Utils::CheckDate($year,$month,$day);
			return ($year."-".$month."-".$day,$day.".".$month.".".$year);	
		}
	}
	if(($year < 1900) || ($year > $year_sys+2000))
	{
		$year = 1900;
	}
	#print "test6";
	return ($year."-".$month."-".$day,$day.".".$month.".".$year);
}
#using for validation region
sub GetRegionOne
{
	my ($data,%dico) = @_;
	my @arr_keys = (keys %dico);
	my @arr = ();
	my @arr_return = ();
	my $result = "";
	my @sorted_keys = sort { length $b <=> length $a } @arr_keys;#purpose: words longer will find first 
	foreach(@sorted_keys)
	{
		my @lstSyn = @{$dico{$_}};
		my @sorted = sort { length $b <=> length $a } @lstSyn;
		#my $lab;
		my $lab = $sorted[0];
		#construire le règle compris le synonymes 
		for(my $i=1; $i<@sorted; $i++)
		{
			$lab = $lab."|".$sorted[$i];
		}
		if( $data =~ /['\\\/\[\]\,.?;:!—\-\(\)\" \t]($lab)[\\\[\]\,.?;:!\(\)\"_\" \t]/i) {
			push @arr,$1;
			push @arr_return,$_;
		}
	}
	#get first item
	if(scalar(@arr) >= 1)
	{
		
		if(scalar(@arr) eq 1)
		{
			$result = $arr_return[0];
		}
		else
		{
			#chosir la région prémiere dans le corpus
			my $pos;
			my $temp = 99999999;
			for(my $i=0; $i<@arr; $i++)
			{
				$pos = index($data,$arr[$i]);
				if($pos > 0)
				{
					if($pos < $temp)
					{
						$temp = $pos;
						$result = $arr_return[$i];
					}
				}	
			}	
		}
	}
	return $result;
}
#Appliquer les règles pour maladies, pathogènes
sub ApplyRuleForSmallDico
{
	#check for stades
	my ($data,$old_data,$tag,%dico) = @_;
	#a variable stocks result temp
	my %result_temp  = ();
	my @arr_keys = (keys %dico);
	my @sorted_keys = sort { length $b <=> length $a } @arr_keys;#purpose: words longer will find first 
	foreach(@sorted_keys)
	{
		my @lstSyn = @{$dico{$_}};
		my @sorted = sort { length $b <=> length $a } @lstSyn;
		#my $lab;
		my $lab = $sorted[0];
		#construire le règle compris le synonymes 
		for(my $i=1; $i<@sorted; $i++)
		{
			$lab = $lab."|".$sorted[$i];
		}
		if($lab =~ /\./gm)
		{
			$lab =~ s/\./\\./gm;#check dans le dictionnaire a le charactère spéciale
		}	
		if( $data =~ /['\\\/\[\]\,.?;:!—\(\)\" \t]($lab)[\\\[\]\,.?;:!\(\)\" \t]/i) {
			#$result_temp{$1}++;
			$data =~ s/(['\\\/\[\]\,.?;:!—\«\(\)\" \t])($lab)([\\\[\]\,.?;:!\»\(\)\" \t])/$1$2 <$tag>$2<\/$tag> $2$3/gi;
			#print $lab."\n";
			#last;
		}
		#if it is the first paragraph
		if( $data =~ /[\{\}]($lab)[\\\[\]\{\},?;:.!\(\)\" \t]/i) {
			#$result_temp{$1}++;
			$data =~ s/([\{\}])($lab)([\\\[\]\{\},?;:.!\«\(\)\" \t])/$1<$tag>$2<\/$tag> $2$3/gi;
			#last;
		}	
	}
	#every dictionary, we must check conflict
	#check conflict in the results if it is contient in the words longer.
	$data = Modules::Structure::CheckConclictTag($data,$old_data,$tag);
	return $data;
}
#car le dictionnaire de ville très gros, alternative trouver chaque mots dans le dictionnaire entre le corpus, je fais contre
sub ApplyRuleForBigDico
{
	my ($data,$old_data,$tag,%dico) = @_;
	my %ListMots          = ();
	my %ListNomDico       = ();
	my %ListCommuneForDoc = ();
	#appliquer n grams
	my $test = "";
	my @words = split /[\{\}.,?;:!\(\) ]/,$data;
	for my $pos (0 .. $#words) {
	  for my $phrase_len (0 .. ($pos >= $N_GRAM ? $N_GRAM - 1 : $pos)) {
	    my $phrase = join ' ', @words[($pos - $phrase_len) .. $pos];
	    $phrase =~ s/^\s+|\s+$//g;
	    if($phrase ne "")
	    {
	    	 $ListMots{$phrase}++;
	    	 #print $phrase."\n";
	    	 $test .= $phrase."\n";	
	    }
	  }
	} 
	#Modules::Utils::SaveFile("./data/temp/test.txt",$test);
	#enlever stop words
	if ( length( $Modules::Parametre::FILE_STOP_WORD || '' )) {
		my @stopwords = Modules::Dico::LoadDicoNameAndSyn($Modules::Parametre::FILE_STOP_WORD);
		foreach(@stopwords)
		{
			delete $ListMots{$_};
		}
	}
	
	my  @ListSyn = ();
	while (my ($key, $value) = each(%dico)){
		@ListSyn           = @{$value};
		foreach(@ListSyn)
		{
			$ListNomDico{$_}++;
		}
	} 
	
	for my $key (keys %ListMots){
		#print $key."\n";
		if(exists $ListNomDico{$key}) {
			$ListCommuneForDoc{$key} = $ListNomDico{$key};
			#print $key."\n";
		}
	}
	#Modules::Utils::PrintHashOfArray(\%ListCommuneForDoc);

	#check for communes
	while (my ($key, $value) = each(%ListCommuneForDoc)){
		#delete \/
		if($data =~  /[\\\[\]\{\}.,?;!—\(\)\" \t]+($key)[\\\[\]\{\}.,?;:!\(\)\" \t]+/i) {
			$data =~ s/([\\\[\]\{\}.,?;!—\(\)\" \t]+)($key)([\\\[\]\{\}.,?;:!\(\)\" \t]+)/$1$2 <$tag>$2<\/$tag> $2$3/gi;
		}
		#if it is a paragraph
		if( $data =~ /[\{\}]+($key)[\\\[\]\{\},?;:.!\(\)\" \t]+/i) {
			$data =~ s/([\{\}]+)($key)([\\\[\]\{\},?;:.!\(\)\" \t]+)/$1<$tag>$2<\/$tag> $2$3 /gi;
			#last;
		}
	}		
	#every dictionary, we must check conflict
	#check conflict in the results if it is contient in the words longer.
	$data = Modules::Structure::CheckConclictTag($data,$old_data,$tag);	
	return $data;
}
#extraire les données, mis à jour les résultats dans un variable global (résulat de tout le document)
#cette function a 3 paramètres:un linge courant, un regle et un variable en contenant de résultat 
sub ExtractDataGlobal
{
	my ($line,$key) = @_;
	my (%r) = %{$_[2]};
	my @myarray;
	my $reg = "<$key>(.*?)<\/$key>";
	@myarray = ($line =~ m/$reg/g);
	if(@myarray >= 1)
	{
		for(my $i=0; $i < @myarray; $i++)
		{
			#trim both ends
			$myarray[$i] =~ s/^\s+|\s+$//g;
			$r{$myarray[$i]}++;#tous les caractères sont lower case
		}
	}
	return %r;
}
#extraire les données, rentrer seullement les resultats de courrant ligne 
sub ExtractDataLocal
{
	my ($line,$reg) = @_;
	my (%r) = ();
	my @myarray;
	@myarray = ($line =~ m/$reg/g);
	if(@myarray >= 1)
	{
		for(my $i=0; $i < @myarray; $i++)
		{
			$r{$myarray[$i]}++;
		}
	}
	return %r;
}
#fonctionc pour faire quoi?
#ajouter la position que il apparait dans le texte orginal
sub ExtractDataParagraphe
{
	my ($line,$reg,$lineOginal) = @_;
	my %r = (%{$_[3]});
	my @myarray;
	
	@myarray = ($line =~ m/$reg/g);
	if(@myarray >= 1)
	{
		for(my $i=0; $i < @myarray; $i++)
		{
			$r{$myarray[$i]} = index($lineOginal,$myarray[$i]);#return -1 if not found
			#print $myarray[$i]."\n";
		}
	}
	return %r;
}
#retouner un ou plusieur résultat de chaque entité
sub FilterResult
{
	my $data = $_[0];
	my (%result) = %{$_[1]};
	my (%type_get) = %{$_[2]};
	
	foreach my $tag (keys %type_get)
	{
		if(lc($type_get{$tag}) ne "all")
		{
			my %temp = %{$result{$tag}};#un hash en stokant les résultat de chaque entité
			if(Modules::Utils::SizeHash(%temp) > $type_get{$tag})#si les résultats sont en supérieur à démander
			{
				#choisir les résultats premières dans le corpus
				my $pos;
				my %test = ();
				foreach my $d (keys %temp)
				{
					$pos = index($$data,">".$d."<",0);
					#print "$d:$pos\n";
					#verifier la date valable ou pas
					if($d =~ m/(\d{1,2})\.(\d{1,2})\.(\d{2,4})/g)
					{
						if(($1 > 31) || ($2 > 12))
						{
							$pos = 1000000;#assigner une position de plus haut
						}
					}
					if($pos >= 0)
					{
						#print $d.":".$pos."\n";
						$test{$d} = $pos;
					}	
				}
				#selections
				my $count = 0;
				my $pre = -1;
				%temp = ();
				foreach my $name (sort { $test{$a} <=> $test{$b} } keys %test) {
					if($count < $type_get{$tag})
					{
						if($pre ne $test{$name})#est meme position
				    	{
				    		$temp{$name}++;
				    		$count++;
				    		$pre = $test{$name};
				    	}	
					}
					else#supprimer du corpus
					{
						$$data =~ s/<$tag>$name<\/$tag>//gi;
					}
			    }
			    #$result{$tag} = \%temp;
			}
			my %test = ();
			foreach my $date (keys %temp)
		    {
		    	#print "old:".$date."\n";
		    	if($date =~ m/(\d{2})\.(\d{1,2})\.(\d{1,4})/g) #ex: 02.07.198
		    	{
		    		my $day = $1;
		    		my $month = $2;
		    		my $year = $3;
		    		#print "day:$day\n";
		    		#print "month:$month\n";
		    		#print "year:$year\n";
		    		if(length($month) eq 1)
		    		{
		    			$month = "0".$2; 
		    		}
		    		#corriger dans le corpus
	    			my $year_current =(localtime)[5];
					$year_current = $year_current+1900;
	    			if(($year < 1900) or ($year > $year_current))
	    			{
	    				my $temp = $$data; 
						my @y =( $temp =~ m/\d{4}/g);
	    				foreach(@y)
	    				{
	    					#print "y_curr:".$_."\n";
	    					if((1900 <= $_) and ($_ <= $year_current))
	    					{
	    						$year = $_;
	    						last;
	    					}
	    				}	
	    			}
		    		$date = $day.".".$month.".".$year;
		    		#print "new:".$date."\n";
		    	}elsif($date =~ m/(\d{1,2})\.(\d{1,4})/g) #ex: 07.198
		    	{
		    		my $month = $1;
		    		my $year = $2;
		    		#print "month:$month\n";
		    		#print "year:$year\n";
		    		if(length($month) eq 1)
		    		{
		    			$month = "0".$1; 
		    		}
	    			my $year_current =(localtime)[5];
					$year_current = $year_current+1900;
					
	    			if(($year < 1900) or ($year > $year_current))
	    			{
	    				my $temp = $$data;
	    				my @y =($temp =~ m/\d{4}/g);
	    				foreach(@y)
	    				{
	    					if((1900 <= $_) and ($_ <= $year_current))
	    					{
	    						#print "y_curr:".$year_current."\n";
	    						$year = $_;
	    						last;
	    					}
	    				}	
	    			}
		    		$date = $month.".".$year;
		    		#print "new:".$date."\n";
		    	}elsif($date =~ m/(\d{4})/g) #ex: 07.198
		    	{
		    		my $year = $1;
		    		my $year_current =(localtime)[5];
					$year_current = $year_current+1900;
		    		if(($year < 1900) or ($year > $year_current))
	    			{
	    				my $temp = $$data;
	    				my @y =($temp =~ m/\d{4}/g);
	    				foreach(@y)
	    				{
	    					if((1900 <= $_) and ($_ <= $year_current))
	    					{
	    						$year = $_;
	    						last;
	    					}
	    				}	
	    			}	
		    	}
		    	$test{$date}++;
		    }
		    $result{$tag} = \%test;
		}
	}
	
	return %result;
}
#filtrer les résultats -> en éliminant des mots de doublons, synonyms
#deux parametres: première dictionnaire, deuxième resultat cru
sub NormaliseResult
{
	my %ouput = ();
	my $rel = $_[0];
	my (%dico) = %{$_[1]};
	my (%input) = %{$_[2]};
	for my $key (keys %input)
	{
		#si le resultat est le même key dans le dictionnaire
		if(exists $dico{$key})
		{
			$ouput{$key}++;
		}
		else
		{
			my $find = 0; #1 si en cherchant est true
			for my $key1(keys %dico)
			{
				$find = 0;
				my @arr = @{$dico{$key1}};
				#changer dans le relation
				foreach(@arr)
				{
					if(lc($_) eq lc($key))
					{
						$ouput{$key1}++;
						#modifier dans les relations
						if(length($_)>1)
						{
							#$Modules::Parametre::RELATION =~ s/[:]($_)[:]/:$key1:/gi;
							$$rel =~ s/[:]($_)[:]/:$key1:/gi;	
						}
						$find = 1;
						last;
					}
				}
				#si trouvée
				if($find eq 1)
				{
					last;
				}
			}
			#si non trouvée dans les dictionnaires
			if($find eq 0)
			{
				$ouput{$key}++;
			}
		}
	}
	return %ouput;
}
#enlever les résultats qui contenent dans la liste blacklist
sub RemoveBlackList
{
	my %results = %{$_[0]};
	my %black_list = %{$_[1]};
	for my $key (keys %black_list)
	{
		#clé est un tag "v" ou "c" ....
		if(exists $results{$key})
		{
			my %tmp = %{$results{$key}};
			my $file = $black_list{"$key"};
			my %list = Modules::Dico::LoadDicoNameAndSyn($file);
			for my $lst (keys %list)
			{
				my @valeurs = @{$list{$lst}};
				foreach(@valeurs)
				{
					if (exists $tmp{$_})
					{
						delete $tmp{$_};
					}
				}
			}
			%{$results{$key}} = %tmp;
		}
	}
	return %results;
}
#=========================================Fin module
1;