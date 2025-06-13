###############################################################################
#Created: PHAN TRONG TIEN                                                     #
#Created date: 20/03/2014													  #
###############################################################################
use JSON;
#use modules defined
use Modules::Dico;
use Modules::Utils;
use Modules::Entite;
use Modules::Relation;
use Modules::Structure;
use Modules::Evaluation;
use Modules::Parametre;

use utf8;
use open(IO => ':encoding(utf8)');
binmode(STDERR, ':utf8');
binmode(STDOUT, ':utf8');
binmode(STDIN, ':utf8');# All output will be UTF-8

#lire le fichier configuration
my $json;
{
  local $/; #Enable 'slurp' mode
  open my $fh, "<", "./config/vespa.json";
  $json = <$fh>;
  close $fh;
}
#Declare variables store results et data
my $config = decode_json($json);
# Output to screen one of the values read
####################################################################
my $FILE_INPUT = "./data/eval/eval_utf8.txt";
my $FILE_OUTPUT = "./data/eval/eval_utf8_norm.txt";
my $FILE_REPLACE = $config->{replace}->{file};
my $FILE_AVOID = $config->{avoid}->{file};
my $FILE_STOP_WORD = $config->{stopword}->{file};
my %DICO = ();
my %TYPE_TROUVE = ();
my %TYPE_DICO = ();
my %TAG = ();
my %BLACK_LIST = ();
#si existant une champe "unitex"
if(exists($config->{"blacklist"}))
{
		my @result = @{$config->{"blacklist"}};
		foreach(@result)
		{
			my %hash = %{$_};
			if((exists($hash{"tag"})) and (exists($hash{"file"})))
			{
				$BLACK_LIST{$hash{"tag"}} = $hash{"file"};#exemple NUI -> n
			}
		}	
}
#lire les dictionnaire dans le fichier config
print "Attendez, S'il vous plaît ....................\n";
my @dico = @{$config->{dico}};
foreach(@dico)
{
	my %info_dico = %{$_};
	my $tag = $info_dico{"tag"};
	my $file = $info_dico{"file"};
	my $node = $info_dico{"node"};
	my $col_key = $info_dico{"col_key"};
	my $col_val = $info_dico{"col_val"};
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
	$TAG{$file} = $tag;
	my %sub_dico = Modules::Dico::LoadDico($file,$node,$col_key,$col_val);
	#Modules::Utils::PrintHashOfArray(%sub_dico);
	$DICO{$file} = \%sub_dico;
}
my $data = "";
open(INPUT,'<:raw:encoding(UTF8)',"$FILE_INPUT") || die "can't read this file: $FILE_INPUT\n";
while(<INPUT>)
{
	#chomp;
	$data = $data.$_;
}
for my $key (keys %TAG)
{
	my %dico = %{$DICO{$key}};
	my %test = ();
	if(exists $BLACK_LIST{$TAG{$key}})
	{
		my $file_black_list = $BLACK_LIST{$TAG{$key}};
		my %list = Modules::Dico::LoadDicoNameAndSyn($file_black_list);
		for my $key (keys %list)
		{
			my @arr = @{$list{$key}};
			foreach(@arr)
			{
				$test{$_}++;
			}
			
		}
		#Modules::Utils::PrintHash(%test);
	}
	for my $key1 (keys %dico)
	{
		@elements = @{$dico{$key1}};
		foreach(@elements)
		{
			#check length et black list
			if(length($_)>1 and (!exists $test{$_}))
			{
				$data =~ s/[:]($_)[:]/:$key1:/gi;	
			}
		} 
	}
}

if ( length( $FILE_REPLACE || ''))
{
	my %mots_alternative = Modules::Dico::LoadDicoNameAndSyn($FILE_REPLACE);
	for my $key (keys %mots_alternative)
	{
		my @elements = @{$mots_alternative{$key}};
		foreach(@elements)
		{
			$data =~ s/[\\\/\[\]\{\}.,?;:!\(\)\" \t]+($_)[\\\/\[\]\{\}.,?;!\(\)\" \t]+/:$key/gi;
		} 
	}
}
=begin
#ajouter deux caractères $$
my $relation = "";#en stokant tous les rélation
if (exists($config->{"relation"}))
{
	my $root = $config->{"relation"}->{"root"};
	my $neg = $config->{"relation"}->{"negation"};
	my @links = @{$config->{"relation"}->{"link"}};
	if ( length( $root || ''))
	{
		my @links = sort { $b cmp $a } @links;
		foreach(@links)
		{
			$data =~ s/[:]($_)[:]/:$_:\$\$:/gi;	
		}
	}
}
#modifier le fois dernier
$data =~ s/[:]\$\$[:]([a-z]):\$\$[:]/:$1:\$\$:/gi;
$data =~ s/  / /gm;
=cut
#supprimer deux espaces avec un espace
$data =~ s/  / /gm;
close(INPUT);
open(OUTPUT,'>:raw:encoding(UTF8)',"$FILE_OUTPUT") || die "Can't open this file: $FILE_OUTPUT";
print OUTPUT $data;
close(OUTPUT);
#print $data;
print "\nFini\n";