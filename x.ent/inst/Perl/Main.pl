###############################################################################
#Created: PHAN TRONG TIEN                                                     #
#Created date: 20/03/2014
#Modified date: 13/07/2014
###############################################################################
use Cwd 'abs_path';
my $dir =    abs_path($0);
$dir =~ s/Perl\/Main\.pl//;

use strict;
use warnings;
use JSON;
use utf8;
use autodie;
use File::Temp qw(tempdir);
use File::Basename;
use File::Copy;
use File::Path qw/make_path/;
use open(IO => ':encoding(utf8)');
binmode(STDERR, ':utf8');
binmode(STDOUT, ':utf8');
binmode(STDIN, ':utf8');# All output will be UTF-8
#use modules defined
use Modules::Dico;
use Modules::Utils;
use Modules::Entite;
use Modules::Relation;
use Modules::Structure;
use Modules::Evaluation;
use Modules::Parametre;
#lire le fichier configuration
my $start_time = time;
#get path of configuration file if it exists in the commandline
my $json_path = $ARGV[0];
my $json;
{
    local $/; #Enable 'slurp' mode
    my $fh;
    if(not defined $json_path)
    {
        open $fh, "<", $dir."/www/config/ini.json";
    }
    else
    {
        open $fh, "<", $json_path;
        #copy current configuration file to configuration file default
        #print $json_path;
        #copy $json_path,$dir."/www/config/ini.json";
    }
    $json = <$fh>;
    close $fh;
}
#Declare variables store results et data
my $config = decode_json($json);

#temp dir
my $dir_temp = tempdir( CLEANUP => 1 );

#chdir $dir_temp;
# Output to screen one of the values read
####################################################################
my $DIR_INPUT = $config->{corpus}->{dir};
#print "Corpus:".$DIR_INPUT."\n";
my $FILE_OUTPUT = $config->{result}->{file};;#$dir_temp."/output.txt";
#print "Output:".$FILE_OUTPUT."\n";
my $FILE_EVAL = $config->{eval}->{file};
#print "Eval:".$FILE_EVAL."\n";
my $FILE_REPLACE = $config->{replace}->{file};
#print "Replace:".$FILE_REPLACE."\n";
my $FILE_AVOID = $config->{avoid}->{file};
my $FILE_STOP_WORD = $config->{stopword}->{file};
if (length($FILE_STOP_WORD || '')) {
  $FILE_STOP_WORD = $dir."dico/".$FILE_STOP_WORD;
}
my %BLACK_LIST = ();
$Modules::Parametre::FILE_STOP_WORD = $FILE_STOP_WORD;
my %DICO = ();
my %TYPE_TROUVE = ();
my %TAG_DICO = ();
my %TYPE_GET= ();
#check file est existant
if ( length( $FILE_EVAL || '')) {
   if(!(-e $FILE_EVAL)){
      print "File evaluation does not exist, please check your configuration!";
     	exit;
	  }
}
my $out_dir = dirname($FILE_OUTPUT);
if(!(-d $out_dir))
{
  make_path($out_dir) || die "Unable to create directory <$!>\n";
}
#lire les dictionnaire dans le fichier config
#print "Please,wait .....\n";
#si existant la configuration de "disco"
if (exists($config->{"dico"}) ){
	my @dico = @{$config->{dico}};
	if(scalar(@dico) > 0)
	{
		foreach(@dico)
		{
			my %info_dico = %{$_};
			my $tag = $info_dico{"tag"};
			my $file = $dir."dico/".$info_dico{"file"};
      #print "dico:".$file."\n";
			my $node = $info_dico{"node"};
			my $col_key = $info_dico{"col_key"};
			my $col_val = $info_dico{"col_val"};
			my $get = $info_dico{"get"};
			$TYPE_GET{$tag} = $get;
			my $filesize = -s $file;
			$filesize = $filesize/(1024*1024);
			#si la taille de fichier est supérieur 1MB, on vais trouver les mots dans le fichier dans le dictionnaire, et sinon en revanche
			if($filesize >= 1)
			{
				$TYPE_TROUVE{$file} = $Modules::Parametre::TROUVE_DIC; #gros data
			}else
			{
				$TYPE_TROUVE{$file} = $Modules::Parametre::TROUVE_CORPUS;
			}
			$TAG_DICO{$file} = $tag;
			my %sub_dico = Modules::Dico::LoadDico($file,$node,$col_key,$col_val);
			#Modules::Utils::PrintHashOfArray(%sub_dico);
			$DICO{$file} = \%sub_dico;
		}
	}
}
#si existant une champe "unitex"
my $TOOL_UNITEX = ();
my $MAIN_GRAPH = ();
my $MY_UNITEX = ();
my @DICO_UNITEX = ();
my %TAG_UNITEX = ();
if (exists($config->{"unitex"}))
{
	if(exists($config->{"unitex"}->{"system"}))
	{
		if(exists($config->{"unitex"}->{"system"}->{"tool_unitex"}))
		{
			$TOOL_UNITEX = $config->{"unitex"}->{"system"}->{"tool_unitex"};
		}
		if(exists($config->{"unitex"}->{"system"}->{"main_graph"}))
		{
			$MAIN_GRAPH = $config->{"unitex"}->{"system"}->{"main_graph"};
		}
		if(exists($config->{"unitex"}->{"system"}->{"my_unitex"}))
		{
			$MY_UNITEX = $config->{"unitex"}->{"system"}->{"my_unitex"};
		}
		if(exists($config->{"unitex"}->{"system"}->{"dico"}))
		{
			my @dico = @{$config->{"unitex"}->{"system"}->{"dico"}};
			foreach(@dico)
			{
				push @DICO_UNITEX,$_;
			}
		}
	}
	if(exists($config->{"unitex"}->{"result"}))
	{
		my @result = @{$config->{"unitex"}->{"result"}};
		foreach(@result)
		{
			my %hash = %{$_};
			if((exists($hash{"tag"})) and (exists($hash{"tag_unitex"})))
			{
				my $tag = $hash{"tag"};
				$TAG_UNITEX{$hash{"tag_unitex"}} = $tag;#exemple NUI -> n
				$TYPE_GET{$tag} =  $hash{"get"};
			}
		}
	}
}
#si existant une champe "unitex"
if(exists($config->{"blacklist"}))
{
    my @result = @{$config->{"blacklist"}};
    foreach(@result)
    {
        my %hash = %{$_};
        if((exists($hash{"tag"})) and (exists($hash{"file"})))
        {
            $BLACK_LIST{$hash{"tag"}} = $dir."dico/".$hash{"file"};#exemple NUI -> n
            #print "Black list:".$dir."dico/".$hash{"file"}."\n";
        }
    }
}
open(OUTPUT,'>:raw:encoding(UTF8)',$FILE_OUTPUT) || die "Can't open this file: $FILE_OUTPUT";
foreach my $fp (glob($DIR_INPUT."/*.{txt,xml}"))
{
  print "File source:\"".$fp."\"\n";
	my $f_name = ( split m{/}, $fp )[-1];
	$f_name =~ s{\.[^.]+$}{};
	my $fn = $f_name;
	$fn =~ s/[\-\_\.]/ /g;

	my $data = $fn." ". Modules::Utils::ReadFile($fp);
    #remove xml tags
    my ($ext) = $fp =~ /(\.[^.]+)$/;
    if($ext eq ".xml")
    {
        $data =~ s|<.+?>||g;
    }
	#prétraitement
	#$data = Modules::Utils::NormailezeData($data);
	
	if ( length( $FILE_REPLACE || '' )) {
  	$data = Modules::Utils::ReplaceWord($data,$dir."dico/".$FILE_REPLACE);
	}
	#avoid
	if ( length( $FILE_AVOID || '' )) {
		$data = Modules::Structure::AvoidPhase($data,$dir."dico/".$FILE_AVOID,\%DICO,\%TAG_DICO);	
	}
	#
	if (($TOOL_UNITEX ne "") && ($MAIN_GRAPH ne "") && ($MY_UNITEX ne ""))
	{
		my $file_temp = $dir_temp."/temp.txt";
		my $dir_snt = $file_temp;
		$dir_snt =~ s/\.txt/_snt/;
		Modules::Utils::SaveFile($file_temp,$data);
		print "Processing Unitex\n";
		my $file_unitex = Modules::Structure::TraitementUnitex($file_temp,$TOOL_UNITEX,$MAIN_GRAPH,$MY_UNITEX,\@DICO_UNITEX,$dir_temp);
		print "Finish processing Unitex\n";
		$data = Modules::Utils::ReadFile($file_unitex);
	}
	print "Processing Dico\n";
	my $old_data = $data;
	#modifier les données
	#verifier le fichier mot vide en existant dans le fichier json
	#replace
	#non dico: Date
	#extract d'information
	#si type = "MAJ", on dois appliquer ce dictionnaire premiere
	for my $key (keys %TAG_DICO)
	{
		if($TYPE_TROUVE{$key} eq $Modules::Parametre::TROUVE_CORPUS)
		{
			$data = Modules::Entite::ApplyRuleForSmallDico($data,$old_data,$TAG_DICO{$key},%{$DICO{$key}});
		}
		else
		{
			$data = Modules::Entite::ApplyRuleForBigDico($data,$old_data,$TAG_DICO{$key},%{$DICO{$key}});
		}
	}
	print "Finish processing dico\n";
	#vérifier des conflits
	#print "$old_data\n";
	$data = Modules::Structure::CheckConfict($data,$old_data,\%TAG_DICO,\%TAG_UNITEX);
	#print "$data\n";
	#récupérer des résultats
	my %result = (); #en stocken le résultat
	my $reg = ();
	my $label = ();
	#en obtenant le résultat
	#format le result tat hash{tag}->hash{list de résultat}
	for my $key (keys %TAG_DICO)
	{
		%{$result{$TAG_DICO{$key}}}= Modules::Entite::ExtractDataGlobal($data,$TAG_DICO{$key},\%{$result{$TAG_DICO{$key}}});
	}
	#en obtenant le résultat dans le Unitex
	#my $data1 = Modules::Utils::ReadFile("$dir_snt/concord.ind");
	for my $key (keys %TAG_UNITEX)
	{
		%{$result{$TAG_UNITEX{$key}}}= Modules::Entite::ExtractDataGlobal($data,$key,\%{$result{$TAG_UNITEX{$key}}});
	}
	#vérifier le résultat routourné
	%result = Modules::Entite::FilterResult(\$data,\%result,\%TYPE_GET);
	
  for my $key (keys %TAG_UNITEX)
	{
		#$key -> CLI->c
		$data =~ s/<$key>/<$TAG_UNITEX{$key}>/g;
		$data =~ s/<\/$key>/<\/$TAG_UNITEX{$key}>/g;
	}
	#en créant les rélations
	my $relation = "";#en stokant tous les rélation
	if (exists($config->{"relation"}))
	{
		#check type of relation extraction
  	my $type = $config->{"relation"}->{"type"};
		my $root = $config->{"relation"}->{"root"};
		my $neg = $config->{"relation"}->{"negation"};
		my @links = @{$config->{"relation"}->{"link"}};
		if ( length( $root || ''))
		{
			#if type == 1 => relation extraction with structure analyse
			my @phrases = ();
			my %final = ();
			if($type == 1)
			{
				print "type: structure\n";
				@phrases = Modules::Structure::TransformerData($data,$root);
				%final = Modules::Relation::CreateRelationParagraph(\@phrases,\$root,\@links,\$neg,\%result);
			}
			else
			{
				my %root_rel = %{$result{$root}};
				my $left = $config->{"relation"}->{"left"};
				my $right = $config->{"relation"}->{"right"};
				print "type: cooccurence, $left, $right \n";
				@phrases = Modules::Structure::Direction($data,\%root_rel,\$root,\$left,\$right);
				%final = Modules::Relation::CreateRelationPhrase(\@phrases,\@links,\$neg);	
			}	
			for my $key (keys %final)
			{
				$relation .= "$f_name:$key\n";
			}
		}
	}
	#en utilisant un variable global pour stoker des relation
	#$Modules::Parametre::RELATION = $relation;
	#normaliser les résultat: les mots est dans autres mots
	for my $key (keys %TAG_DICO)
	{
		%{$result{$TAG_DICO{$key}}}= Modules::Entite::NormaliseResult(\$relation,\%{$DICO{$key}},\%{$result{$TAG_DICO{$key}}},$key);
	}
	#remove blacklist
	%result = Modules::Entite::RemoveBlackList(\%result,\%BLACK_LIST);
	#récupérer le résultat après d'appler la fonction Normalise
	#$relation = $Modules::Parametre::RELATION;
	#filter des relation doublon
	my @arr = split /[\n]/,$relation;
	my %rel = ();
	foreach(@arr)
	{
		$rel{$_}++;
	}
	#sauvergarder le resultat du fichier
	for my $key (keys %result)
	{
		if(Modules::Utils::SizeHash(%{$result{$key}}) > 0 )
		{
			$label = $f_name.":$key:"."\$:".Modules::Utils::HashToString(%{$result{$key}});
			print OUTPUT $label."\n";
		}
	}
	#savergarder les rélations
	for my $key (keys %rel)
	{
		print OUTPUT $key."\r\n";
	}	
	print OUTPUT "\n";
}
close(OUTPUT);
#Evalution============================================================
if ( length( $FILE_EVAL || '') and length( $FILE_OUTPUT || '')) {
	    #Modules::Evaluation::EvaluationLocal($FILE_EVAL,$FILE_OUTPUT);
	    Modules::Evaluation::EvaluationGlobal($FILE_EVAL,$FILE_OUTPUT);
}
# Do stuff
my $duration = time - $start_time;
print "Execution time: $duration s\n";
print "Finish, thanks\n";
########################################################################