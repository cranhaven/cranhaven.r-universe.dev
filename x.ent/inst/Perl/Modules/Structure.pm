#crée par: PHAN Trong Tien
#date: 12/05/2014
#===========================================
package Modules::Structure;
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
my $END = "end";
my $PHRASE = "phrase";
my $BRACKET = "bracket";
sub TransformerData
{
	my ($data,$root) = @_;
	my @final = ();
	my @sentences = split m/(?<=[.!?;])/m, $data;
	my $mark = 0;
	#my $reg = "{S}(.*?) <$root>(.*?)<\/$root>";
	my $reg = "{S}<$root>(.*?)<\/$root>";
	#my $reg1 = "<$root>([A-ZÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝ\\s]*)<\/$root>";
	my $reg1 = "<$root>([A-ZÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝ\\s]+[\\w\\s]*)<\/$root>";
	my $phrase = "";
	$mark = 0;
	my $mark_old = 1;
	foreach (@sentences)
	{
		if($_ =~ /($reg|$reg1)/g)
		{
			$mark += 1;
			#print $1."\n"; 
		}
		if($mark > $mark_old)
		{
			push @final,$phrase;
			$phrase = "";
			$mark_old = $mark;
		}
		$phrase .= $_;
	}
	if(length($phrase) > 0)
	{
		push @final,$phrase;
	}
	return @final;
}
sub AvoidPhase
{
	my ($data,$fichier) = @_;
	my %dico = %{$_[2]};
	my %tag_dico = %{$_[3]};
	my %phases = Modules::Dico::LoadDicoNameAndSyn($fichier);
	#chaque phrase a le format: debut -> stop(tag , phrase, end of document,)
	#foreach my $key (keys %phases)
	#{
	#	$data =~ s/$key(.*?)\.//g;
	#}
	foreach my $key (keys %phases)
	{
		my @arr = @{$phases{$key}};
		if(scalar(@arr) >= 2)
		{
			#quand le fini a pluisieurs choix: ex phase, end ...
			#priorité élément primier
			for(my $i=1;$i<scalar(@arr);$i++)
			{
				if($arr[$i] eq $PHRASE)
				{
					if($data =~ m/$key(.*?)\./g)
					{
						$data =~ s/$key(.*?)\./\./g;
						#print "$key(.*?)\.\n"; 	
					}
				}elsif($arr[$i] eq $BRACKET)
				{
					if($data =~ m/$key(.*?)\)/g)
					{
						$data =~ s/$key(.*?)\)//g;
						#print "$key(.*?)\)\n"; 	
					}
				}elsif($arr[$i] eq $END)
				{
					if($data =~ m/$key(.*?)$fin/g)
					{
						$data =~ s/$key(.*?)$fin//g;
						#print "$key(.*?)$fin\n"; 	
					}
				}else
				{
					my %plante = ();
					#trouver dans dictionnaire
					for my $tag (keys %tag_dico)
					{
						if($tag_dico{$tag} eq $arr[$i])
						{
							my %dico_tag = %{$dico{$tag}};
							#Modules::Utils::PrintHashOfArray(%dico_tag);
							for my $elm (keys %dico_tag)
							{
								my @arr_elm = @{$dico_tag{$elm}};
								my $reg = "";
								$reg = $arr_elm[0];
								for(my $i=1;$i<@arr_elm;$i++)
								{
									$reg .= "|".$arr_elm[$i];
									my @myarray = ($data =~ /($reg)/gi);
									foreach (@myarray)
									{
										if (Modules::Utils::CheckUpCase($_) eq 1)
										{
											$plante{$_}++;
										}
									}		
								}
							}	
						}
					}
					#obtenir la posision plus proche avec le position de key
					#en trouvant début clé
					
					my $pos_key = index($data,$key);
					my $tag_fin = "";
					my $min = "999999999999999";
					for my $p (keys %plante)
					{
						my $pos = index($data,"{S}".$p,$pos_key);
						#comparer pour prend la position plus porche
						if($pos > $pos_key)
						{
							if($pos < $min)
							{
								$min = $pos;
								$tag_fin = $p;
								#print $p."\n"; 
							} 
						}
					}
					#modifier le $key si il a des caractère spéciale
					$key =~ s/\./\\\./;
					if($tag_fin ne "")
					{
						if($data =~ m/$key(.*?)($tag_fin)/g)
						{
							$data =~ s/$key(.*?)($tag_fin)/{S}$2/g;
							#print "$key$1$tag_fin\n";
							#last;
						}
					}		
				}
			}
		}
		else
		{
			#défaut en enlévant du mot debut au mot point (.))
			$data =~ s/$key(.*?)\.//g;
		}
	}
	return $data;
}
sub CheckConfict
{
	my $result = "";
	#eval
	#{
		#extraction tous les donnés dans les balises
		#et appliquer un algorithme pour trouver un mots est dans un autre mots
		my ($curr_data,$old_data) = @_;
		my (%tag_dico) = %{$_[2]};
		my %tag_unitex = %{$_[3]};
		#Modules::Utils::PrintHash(%tag_dico);
		#couper chaque phrase
		my @line_curr_data = split(/\.\{S}/,$curr_data);
		my @line_old_data = split(/\.\{S}/,$old_data);
		#print scalar(@line_curr_data)."\t".scalar(@line_old_data)."\n";
		my $total_line = (scalar(@line_curr_data) > scalar(@line_old_data))?scalar(@line_old_data):scalar(@line_curr_data);
		for(my $n_line = 0; $n_line<$total_line;$n_line++)
		{
			#print $line_curr_data[$n_line]."\n";
			#print $line_old_data[$n_line]."\n";
			my %test = ();
			foreach my $key (keys %tag_dico)
			{
				my $reg = "<$tag_dico{$key}>(.*?)<\/$tag_dico{$key}>";
				%test = Modules::Entite::ExtractDataParagraphe($line_curr_data[$n_line],$reg,$line_old_data[$n_line],\%test);
			}
			foreach my $key (keys %tag_unitex)
			{
				my $reg = "<$key>(.*?)<\/$key>";
				%test = Modules::Entite::ExtractDataParagraphe($line_curr_data[$n_line],$reg,$line_old_data[$n_line],\%test);
			}
			#vérifier le première, le format de donnée: <NUI>entre 1 et 10 pucerons <b>pucerons</b> pucerons par plante</NUI> ou <STA>diamètre de pomme <p>pomme</p> pomme supérieur à 25 cm</STA> ....
			my @temp = ();
			foreach my $key (keys %test)
			{
				#why can i do that?
				#$key =~ s/[^A-Za-z0-9A-Za-zÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïñòóôõöùúûüýÿ\%\-\.\,\/]//gm;
				if(length(Modules::Utils::Trim($key)) > 1 )
				{
					push @temp,$key;	
				}
			}
			#Modules::Utils::PrintArray(@temp);
			#print "$line_curr_data[$n_line].\n";
			#print "pommes".$test{"pommes"}."\n";
			#Modules::Utils::PrintHash(%test);
			#Modules::Utils::PrintArray(@temp);
			
			#short items in array
			my @sorted_temp = sort { length $b <=> length $a } @temp;
			my $items = scalar(@sorted_temp);
			for(my $i = 0;$i < $items - 1; $i++)
			{
				for(my $j = $items -1; $j > $i ; $j--)
				{
					#print $sorted_temp[$i]."\t-\t".$sorted_temp[$j]."\n";
					my $test1 = $sorted_temp[$j];
					$test1 =~ s/\(/\\(/g;
					$test1 =~ s/\)/\\)/g;
					$test1 =~ s/[\[#\-%&\$*+()\]]//g;
					#print $test1."\n";
					if($sorted_temp[$i] =~ m/$test1/)
					{
						if( exists $test{$sorted_temp[$j]} and exists $test{$sorted_temp[$i]})
						{
							#print $sorted_temp[$i]."\t".$sorted_temp[$j]."\t"."$sorted_temp[$court] =>".$test{$sorted_temp[$court]}."\t"."$sorted_temp[$i] =>".$test{$sorted_temp[$i]}."\t"."\n";
							#vérifier le première, le format de donnée: <NUI>entre 1 et 10 pucerons <b>pucerons</b> pucerons par plante</NUI> ou <STA>diamètre de pomme <p>pomme</p> pomme supérieur à 25 cm</STA> ....
							if(($test{$sorted_temp[$j]} >= 0) and ($test{$sorted_temp[$i]} eq -1))
							{
								#print $sorted_temp[$i]."\t".$sorted_temp[$j]."\t"."$sorted_temp[$j] =>".$test{$sorted_temp[$j]}."\t"."$sorted_temp[$i] =>".$test{$sorted_temp[$i]}."\t"."\n";
								my $reg_del = "<[A-Za-z]+>$sorted_temp[$j]<\/[A-Za-z]+> $sorted_temp[$j] ";
								#print "$reg_del\n";
								#print "1. $line_curr_data[$n_line]\n";
								$line_curr_data[$n_line] =~ s/$reg_del//gi;
								#print "2. $line_curr_data[$n_line]\n";
							}
							if(($test{$sorted_temp[$j]} >= 0) and ($test{$sorted_temp[$i]} >= 0) and ($test{$sorted_temp[$j]}>=$test{$sorted_temp[$i]}) and ($test{$sorted_temp[$j]} + length($sorted_temp[$j])<=$test{$sorted_temp[$i]}+ length($sorted_temp[$i])))
							{
								#print "deux mots sont \"$sorted_temp[$i]\": $test{$sorted_temp[$i]} et \"$sorted_temp[$j]\":$test{$sorted_temp[$j]}.\n";
								my $reg_del = "<[A-Za-z]+>$sorted_temp[$j]<\/[A-Za-z]+> $sorted_temp[$j]";
								#print $reg_del."\n";
								$line_curr_data[$n_line] =~ s/$reg_del//g;
							}	
						}	
					}
				}
			}
			$result .= $line_curr_data[$n_line].".{S}";
			#print "$line_curr_data[$n_line].\n\n";
			#print "\n";
		}
		$result  =~ s/\s\s+/ /gm;	
	#};
	#if (my $e = $@) {
    #	print("Something went wrong: $e\n");
	#}
	
	return $result;
}
#check conflict follow every tag
sub CheckConclictTag
{
	my ($curr_data,$old_data,$tag) = @_;
	#couper chaque phrase
	my @line_curr_data = split(/\.\{S}/,$curr_data);
	my @line_old_data = split(/\.\{S}/,$old_data);
	my $result = "";
	#eval
	#{
		my $total_line = (scalar(@line_curr_data) > scalar(@line_old_data))?scalar(@line_old_data):scalar(@line_curr_data);
		for(my $n_line = 0; $n_line<$total_line;$n_line++)
		{
			my %test = ();
			my $reg = "<$tag>(.*?)<\/$tag>";
			%test = Modules::Entite::ExtractDataParagraphe($line_curr_data[$n_line],$reg,$line_old_data[$n_line],\%test);
			#vérifier le première, le format de donnée: <NUI>entre 1 et 10 pucerons <b>pucerons</b> pucerons par plante</NUI> ou <STA>diamètre de pomme <p>pomme</p> pomme supérieur à 25 cm</STA> ....
			my @temp = ();
			foreach my $key (keys %test)
			{
				if(length(Modules::Utils::Trim($key)) > 1 )
				{
					push @temp,$key;	
				}
			}
			#short items in array
			my @sorted_temp = sort { length $b <=> length $a } @temp;
			my $items = scalar(@sorted_temp);
			for(my $i = 0;$i < $items - 1; $i++)
			{
				for(my $j = $items -1; $j > $i ; $j--)
				{
					my $test1 = $sorted_temp[$j];
					$test1 =~ s/\(/\\(/g;
					$test1 =~ s/\)/\\)/g;
					if($sorted_temp[$i] =~ m/$test1/)
					{
						if( exists $test{$sorted_temp[$j]} and exists $test{$sorted_temp[$i]})
						{
							#vérifier le première, le format de donnée: <NUI>entre 1 et 10 pucerons <b>pucerons</b> pucerons par plante</NUI> ou <STA>diamètre de pomme <p>pomme</p> pomme supérieur à 25 cm</STA> ....
							if(($test{$sorted_temp[$j]} >= 0) and ($test{$sorted_temp[$i]} eq -1))
							{
								my $reg_del = "<[A-Za-z]+>$sorted_temp[$j]<\/[A-Za-z]+> $sorted_temp[$j] ";
								$line_curr_data[$n_line] =~ s/$reg_del//gi;
							}
							if(($test{$sorted_temp[$j]} >= 0) and ($test{$sorted_temp[$i]} >= 0) and ($test{$sorted_temp[$j]}>=$test{$sorted_temp[$i]}) and ($test{$sorted_temp[$j]} + length($sorted_temp[$j])<=$test{$sorted_temp[$i]}+ length($sorted_temp[$i])))
							{
								my $reg_del = "<[A-Za-z]+>$sorted_temp[$j]<\/[A-Za-z]+> $sorted_temp[$j] ";
								$line_curr_data[$n_line] =~ s/$reg_del//g;
							}	
						}	
					}
				}
			}
			$result .= $line_curr_data[$n_line].".{S}";
		}
		$result  =~ s/\s\s+/ /gm;
	#};
	#if (my $e = $@) {
    #	print("Something went wrong: $e\n");
	#}
	return $result;	
}
sub Direction
{
	my @result = ();
	my $source = $_[0];
	my %entity = %{$_[1]};
	my $root = ${$_[2]};
	my $left = ${$_[3]};
	my $right = ${$_[4]};
	$source =~ s/{S}//g;
	my $offset = 0;
	my $len = length($source);
	#$left = 50;
	#$right = 100;
	foreach my $key  (keys %entity)
	{
		my $find = "<$root>".$key."</$root>";
		my $pos = 0;
		while(1)
		{
			$pos = index($source,$find,$pos);
			#print "key = $find, pos = $pos\n";
			last if($pos < 0);
			#print $pos."\n";
			#cut every string between current possition - left, current possition + right
			my $pos_left = 0;
			if($pos - $left > 0)
			{
				$pos_left = $pos - $left - length($key)-length($root) - 1;
			}
			my $cut_length = $len - $pos;#default cut to 
			if($pos + $right < $len)
			{
				if($pos_left > 0)
				{
					$cut_length = $left + $right + length($key)*3 + length($root)+9;
				} 
				else
				{
					$cut_length = $pos +$right + length($key)*2 + length($root)+7;  	
				}
			}
			my $data = substr $source, $pos_left, $cut_length;
			print $data."\n";
			push @result,$data;
			$pos++;
		}
	}

	return @result;
}
sub TraitementUnitex
{
    my ($file_source,$tool_unitex,$main_graph,$my_unitex) = @_;
    my @dico_unitex = @{$_[4]};
    my $dir_temp = $_[5];
    my $file_ex = $dir_temp."/test.txt";
    my $dir_snt = $file_source;
    $dir_snt =~ s/\.txt/_snt/;

    my $text_snt = $file_source;
    $text_snt =~ s/\.txt/.snt/;
   
    my $sentence_graphe = $my_unitex."/Graphs/Preprocessing/Sentence/Sentence.grf";
    my $replace_graphe =  $my_unitex."/Graphs/Preprocessing/Replace/Replace.grf";

    my $main_graphe_fst2 = $main_graph;
    $main_graphe_fst2 =~  s/\.grf/.fst2/;
   
    my $sentence_graphe_fst2 = $sentence_graphe;
    $sentence_graphe_fst2 =~  s/\.grf/.fst2/;
   
    my $replace_graphe_fst2 = $replace_graphe;
    $replace_graphe_fst2 =~  s/\.grf/.fst2/;
    #création du répertoire du texte du texte traité s'il n'existe pas déjà
    if(not -d "$dir_snt")
    {
        mkdir($dir_snt);
    }
    #Normalise
    system("\"$tool_unitex\" Normalize \"$file_source\" -r \"$my_unitex/Norm.txt\" -qutf8-no-bom > $file_ex");
    #sentence: copilation du graph, aplllaissement and appliquant
    system("\"$tool_unitex\" Grf2Fst2 \"$sentence_graphe\" -y --alphabet=\"$my_unitex/Alphabet.txt\" -d \"$main_graph/Graphs\">> $file_ex");
    system("\"$tool_unitex\" Flatten \"$sentence_graphe_fst2\" --rtn -d10 -qutf8-no-bom >> $file_ex");
    system("\"$tool_unitex\" Fst2Txt -t\"$text_snt\" \"$sentence_graphe_fst2\" -a\"$my_unitex/Alphabet.txt\" -M >> $file_ex");
    #replace: compilation du graphe
    system("\"$tool_unitex\" Grf2Fst2 \"$replace_graphe\" -y --alphabet=\"$my_unitex/Alphabet.txt\" >> $file_ex");
    #system("$tool_unitex Flatten $replace_graphe_fst2 --rtn -d10 -qutf8-no-bom >> test.txt");
    system("\"$tool_unitex\" Fst2Txt -t\"$text_snt\" \"$replace_graphe_fst2\" -a\"$my_unitex/Alphabet.txt\" -R >> $file_ex");
    #en utilisant le tool Flattent Ce programme prend une grammaire .fst2 en paramètre, et essaye de la transformer en un transducteur à états finis
    #applatissement du graphe
    #system("$tool_unitex Flatten $main_graphe_fst2 --rtn -d10 -qutf8-no-bom >> test.txt");
    #Decoupage en token
    system("\"$tool_unitex\" Tokenize \"$text_snt\" -a\"$my_unitex/Alphabet.txt\" -qutf8-no-bom >> $file_ex");
    # Application du dictionnaire français
    #print $dico_unitex."\n";
    my $dico = "";
    foreach (@dico_unitex)
    {
        $dico .= "\"$_\" ";
    }
    #print "\"$tool_unitex\" Dico \"-t$text_snt\" \"-a$my_unitex/Alphabet.txt\" $dico -qutf8-no-bom >> $file_ex";
    system("\"$tool_unitex\" Dico -t \"$text_snt\" -a \"$my_unitex/Alphabet.txt\" $dico -qutf8-no-bom >> $file_ex");   
    #system("$tool_unitex Dico -t\"$text_snt\" -a $my_unitex/Alphabet.txt $dico_unitex");
    #system("$tool_unitex Dico -t\"$text_snt\" -a $my_unitex/Alphabet.txt /Volumes/MacOS/Users/phantrongtien/unitex/French/Dela/dela-fr-public.bin");
    #system("$tool_unitex Dico -t\"$text_snt\" -a $my_unitex/Alphabet.txt /Volumes/MacOS/Users/phantrongtien/unitex/French/Dela/Delaf_Toponyme_Departement_France_FR_utf8.bin");
    #system("/Users/phantrongtien/Documents/Unitex/UnitexToolLogger Dico -t/Volumes/Data/Code/Java/VESPA/data/temp/temp.snt -a/Users/phantrongtien/unitex/French/Alphabet.txt /Volumes/MacOS/Users/phantrongtien/unitex/French/Dela/dela-fr-public.bin /Volumes/MacOS/Users/phantrongtien/unitex/French/Dela/Delaf_Communes_France_FR_utf8.bin /Volumes/MacOS/Users/phantrongtien/unitex/French/Dela/Delaf_Toponyme_Departement_France_FR_utf8.bin /Volumes/MacOS/Users/phantrongtien/unitex/French/Dela/Delaf_Toponyme_Region_France_FR_utf8.bin");
    #correct erreue
    system("\"$tool_unitex\" SortTxt \"$dir_snt/dlf\" -l\"$dir_snt/dlf.n\" -o\"$my_unitex/Alphabet_sort.txt\" -qutf8-no-bom >> $file_ex");
    system("\"$tool_unitex\" SortTxt \"$dir_snt/dlc\" -l\"$dir_snt/dlc.n\" -o\"$my_unitex/Alphabet_sort.txt\" -qutf8-no-bom >> $file_ex");
    system("\"$tool_unitex\" SortTxt \"$dir_snt/err\" -l\"$dir_snt/err.n\" -o\"$my_unitex/Alphabet_sort.txt\" -qutf8-no-bom >> $file_ex");
    system("\"$tool_unitex\" SortTxt \"$dir_snt/tags_err\" -l\"$dir_snt/tags_err.n\" -o\"$my_unitex/Alphabet_sort.txt\" -qutf8-no-bom >> $file_ex");
    #compilation du graphe
    system("\"$tool_unitex\" Grf2Fst2 \"$main_graph\" -y --alphabet=\"$my_unitex/Alphabet.txt\" >> $file_ex");
    # Application du graphe
    system("\"$tool_unitex\" Locate -t\"$text_snt\" \"$main_graphe_fst2\" -a\"$my_unitex/Alphabet.txt\" -L -M --all -b -Y -qutf8-no-bom>> $file_ex");
    # Génération du texte annoté
    system("\"$tool_unitex\" Concord \"$dir_snt/concord.ind\" -m \"$file_source\" -qutf8-no-bom >> $file_ex");
    return $file_source;
}
#=========================================Fin module
1;
