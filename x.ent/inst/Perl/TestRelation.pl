###############################################################################
#Created: PHAN TRONG TIEN                                                     #
#Created date: 11/06/2014													  #
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
##########################################################################
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
my $FILE_OUTPUT = $config->{result}->{file};
my $FILE_EVAL = $config->{eval}->{file};
if (exists($config->{"relation"}))
{
	my $root = $config->{"relation"}->{"root"};
	my $neg = $config->{"relation"}->{"negation"};
	my @links = @{$config->{"relation"}->{"link"}};
	if ( length( $root || ''))
	{	
		
	}
}
##################CALCULER############################
my %count_X = ();
my %count_Y = ();
my %count_X_Y = ();
my %data_eval = Modules::Evaluation::LoadFile($FILE_EVAL);
my %data_result = Modules::Evaluation::LoadFile($FILE_OUTPUT);
Modules::Utils::PrintHashOfArray(%data_eval);
