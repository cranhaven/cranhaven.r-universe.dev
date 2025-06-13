###############################################################################
#Created: PHAN TRONG TIEN                                                     #
#Created date: 20/03/2014													  #
###############################################################################
#use modules defined
use Modules::Dico;
use Modules::Utils;
use Modules::Entite;
use Modules::Relation;
use Modules::Structure;
use Modules::Evaluation;
use Modules::Parametre;

# All output and input will be UTF-8
use utf8;
use open(IO => ':encoding(utf8)');
binmode(STDERR, ':utf8');
binmode(STDOUT, ':utf8');
binmode(STDIN, ':utf8');

#folder will be store sql files
my $DIR_SQL = "./data/sql/";
#path corpus
my $DIR_CORPUS = "/srv/lisis-lab/web/vespa/reportsOCR";#"/Volumes/Data/reportsOCR";
#path result
#my $FILE_RESULT = "/Volumes/Data/Code/Java/VESPA/data/out/output.txt";
my $FILE_RESULT = "/srv/lisis-lab/devroot/home/tien/R/x86_64-pc-linux-gnu-library/3.1/x.ent/out/output.txt";#"/Users/phantrongtien/Desktop/output.txt";#$config->{result}->{file};
my $path_region = "/srv/lisis-lab/devroot/home/tien/R/x86_64-pc-linux-gnu-library/3.1/x.ent/dico/dico-r_v2.txt";#"/Volumes/Data/Code/Java/VESPA/dico/v3/dico-r_v2.txt";
#my $path_log = "/Users/phantrongtien/Desktop/log.txt";
#open(LOGFILE,'>:raw:encoding(UTF8)',$path_log) || die "Can't open this file: $path_log";
#my $str_log = '';
my $node = "false";#if it is true, exist roots and leaves 
my $col_key = "5";
my $col_val = "4..*";
my %dico_region = Modules::Dico::LoadDico($path_region,$node,$col_key,$col_val);
#my $fn = "AA GC Bourgogne Franche comte 1982 012";
#print Modules::Entite::GetRegionOne($fn,%dico_region);
#read result file (.txt) to a hash in perl
my %data_result = Modules::Evaluation::LoadFile($FILE_RESULT);
#Load dictionaries that you ran CreateCSV.pl
#using a hash to store name of tables (the same name of table in BDD) and path of csv files
my %dico_csv;
$dico_csv{"area"} = "./data/csv/dico-r_v2.csv";#change here if necessary
$dico_csv{"plant"} = "./data/csv/dico-p_v3.csv";#change here if necessary
$dico_csv{"disease"} = "./data/csv/dico-m_v3.csv";#change here if necessary
$dico_csv{"bioagressor"} = "./data/csv/dico-b_v3.csv";#change here if necessary
#créer la table unique (Id,Nom)
for my $name (keys %dico_csv)
{
	my $f_name = $dico_csv{$name};
	my %dico_data = Modules::Dico::LoadDicoCSV($f_name);
	my $FILE_OUTPUT = $DIR_SQL.$name.".sql";
	open(OUTPUT,'>:raw:encoding(UTF8)',$FILE_OUTPUT) || die "Can't open this file: $FILE_OUTPUT";
	my $data = "";#"Id;Nom\n";
	$data .= "Delete from $name;\n";
    $data .= "Insert into $name(Id,Name) values ('0','');\n";#value empty
    for my $key_nom (keys %dico_data)
	{
		my $id = $dico_data{$key_nom};
		$key_nom =~ s/'/''/g;
		if(length($key_nom) > 0)
		{
			$data .= "Insert into $name(Id,Name) values ('".$id."','".$key_nom."');\n";	
		}
	}
	print OUTPUT $data;
	close(OUTPUT);
}

my %dico_plant = Modules::Dico::LoadDicoCSV($dico_csv{"plant"});
my %dico_disease = Modules::Dico::LoadDicoCSV($dico_csv{"disease"});
my %dico_bioagressor = Modules::Dico::LoadDicoCSV($dico_csv{"bioagressor"});
my %dico_area = Modules::Dico::LoadDicoCSV($dico_csv{"area"});

#Create Report.csv and Report.sql from the corpus and the output.txt file
my $FILE_REPORT_CSV = "./data/csv/Report.csv";
my $FILE_REPORT_SQL = $DIR_SQL."report.sql";
open(OUTPUT_CSV,'>:raw:encoding(UTF8)',$FILE_REPORT_CSV) || die "Can't open this file: $FILE_REPORT_CSV";
open(OUTPUT_SQL,'>:raw:encoding(UTF8)',$FILE_REPORT_SQL) || die "Can't open this file: $FILE_REPORT_SQL";

my $data = ();#"Id;Name;Date;Id_Area;Content\n";
my $sql = "Delete from report;\n";
my $id = 0;
#get current date for comparing date of extraction
my ($sec,$min,$hour,$mday_sys,$mon_sys,$year_sys,$wday,$yday,$isdst) =  localtime(time);
$year_sys %= 100; #example: 2015 => $year_sys = 115
foreach my $fp (glob("$DIR_CORPUS/*.{txt,xml}"))
{
	$id += 1;
	my $text = Modules::Utils::ReadFile($fp);
	#prétraitement
	$text =~ s/'//g;
	#print $data."\n";
	my $f_name = ( split m{/}, $fp )[-1];
	$f_name =~ s{\.[^.]+$}{};
	my $fn = $f_name;#remove special characters 
	#en obtenant la date
	#print "$fp\n";
	my @date = ();
	if(exists $data_result{lc($f_name).":d:"})
	{
		@date = @{$data_result{lc($f_name).":d:"}};	
	}
	else
	{
		#print "Non date:".$f_name."\n";
	}
	#en obtenant la region
	my @region = ();
	if(exists $data_result{lc($f_name).":r:"})
	{
		@region = @{$data_result{lc($f_name).":r:"}};
	}
	else
	{
		#print "Non region:".$f_name."\n";
		#if not exist a region then find in the name file
		$fn =~ s/[\_\.]/ /g;
		my $test = Modules::Entite::GetRegionOne($fn,%dico_region);
		#if not found then find in document
		if($test eq "")
		{
			$test = Modules::Entite::GetRegionOne($text,%dico_region);
		}
		push @region,$test;
	}
	#prendre seulement des région dans le dictionnaire
	my @region_new = ();
	foreach(@region)
	{
		#Modules::Utils::PrintHash(%dico_region);
		if(exists $dico_area{lc($_)})
		{
			push @region_new,lc($_);
		}
		else
		{
			#$str_log .= "$f_name: => @region\n";
			#print "$f_name: => @region\n";
		}
	}
	my $id_region = 0;
	#si selement une region
	if(scalar(@region_new) >= 1)
	{
		if(scalar(@region_new) eq 1)
		{
			$id_region = $dico_area{lc($region_new[0])};
			#print "$f_name:$id_region/@region_new\n";
		}
		else
		{
			#chosir la région prémiere dans le corpus
			my $pos;
			my $nom = "";
			my $temp = 99999999;
			my $data = lc($fn." ".$text);
			foreach(@region_new)
			{
				$pos = index($data,$_);
				#in case Haute-Normandie but in document: Haute Normandie 
				if($pos eq -1)
				{
					my $test = $_;
					$test =~ s/[\-]/ /g;
					$pos = index($data,$test);
				} 
				if($pos > 0)
				{
					if($pos < $temp)
					{
						$temp = $pos;
						$nom = $_;
					}
				}	
			}
			if(exists($dico_area{$nom}))
			{
				$id_region = $dico_area{$nom};	
			}
			
		}	
	}
	#check and correct date if it is incorrect
	if(scalar(@date) > 0)
	{
		#12.09.1961
		my $temp_date = $date[0];
		if($temp_date =~ m/(\d{2})\.(\d{1,2})\.(\d{1,4})/g)
	    {
	        my $year = $3;
	        my $month = $2;
	        my $day = $1;	
	        my $date_correct = $year."-".$month."-".$day;
	        my $date_str = $day.".".$month.".".$year;
	        if(length($year) < 4)
	        {
	        	if(length($year) == 2)
		        {
		        	if($year > 0 && $year < $year_sys)
		        	{
		        		$year += 2000;
		        		($year,$month,$day) = Modules::Utils::CheckDate($year,$month,$day);
		        		$date_correct = $year."-".$month."-".$day;
		        		$date_str = $day.".".$month.".".$year;
		        		#print $f_name."-current-".$temp_date."-current-".$date_correct."\n";
		        	}
		        	else
		        	{
		        		#get date from the name of path
		        		($date_correct,$date_str)  =  Modules::Entite::GetOneDate($f_name);
	        			#print $f_name."-current-".$temp_date."-current-".$date_correct."\n";
		        	}
		        }
		        else
		        {
		        	($date_correct,$date_str)  =  Modules::Entite::GetOneDate($f_name);
	        		#print $f_name."-current-".$temp_date."-current-".$date_correct."\n";	
		        }
	        }
	        else #check date validatation 
	        {
	        	($year,$month,$day) = Modules::Utils::CheckDate($year,$month,$day);
            	$date_correct = $year."-".$month."-".$day;		
            	$date_str = $day.".".$month.".".$year;
	        }
	        $sql .= "Insert Into report(Id,Name,Date,Datestr,Id_Area,Content) values ('".$id."','".$f_name."','".$date_correct."','".$date_str."','".$id_region."','".$text."');\n";
	    }
	    elsif($temp_date =~ m/(\d{1,2})\.(\d{1,4})/g)#04.1962
	    {
		    my $year = $2;
		    my $month = $1;
		    my $day = "01";
		    my $date_correct = $year."-".$month."-".$day;
		    my $date_str = $day.".".$month.".".$year;
	        if(length($year) < 4)
	        {
	        	if(length($year) == 2)
		        {
		        	if($year > 0 && $year < $year_sys)
		        	{
		        		$year += 2000;
		        		($year,$month,$day) = Modules::Utils::CheckDate($year,$month,$day);
		        		$date_correct = $year."-".$month."-".$day;
		        		$date_str = $day.".".$month.".".$year;
		        		#print $f_name."-current-".$temp_date."-correct-".$date_correct."\n";
		        	}
		        	else
		        	{
		        		#get date from the name of path
		        		($date_correct,$date_str) =  Modules::Entite::GetOneDate($f_name);
	        			#print $f_name."-current-".$temp_date."-correct-".$date_correct."\n";
		        	}
		        }
		        else
		        {
		        	($date_correct,$date_str) =  Modules::Entite::GetOneDate($f_name);
	        		#print $f_name."-current-".$temp_date."-correct-".$date_correct."\n";	
		        }
	        }
		    $sql .= "Insert Into report(Id,Name,Date,Datestr,Id_Area,Content) values ('".$id."','".$f_name."','".$date_correct."','".$date_str."','".$id_region."','".$text."');\n";
	    }
	}
	else
	{
		#if not existing a date
		#begin: find out in file name
		($date_correct,$date_str) =  Modules::Entite::GetOneDate($f_name);
		if($date_correct eq "1900-01-01")
		{
			#the last, find in document
			($date_correct,$date_str) = Modules::Entite::GetOneDate($text);
			#print $f_name."-correct-".$date_correct."\n";
		}
		#print $f_name."-correct-".$date_correct."\n";
		$sql .= "Insert Into report(Id,Name,Date,Datestr,Id_Area,Content) values ('".$id."','".$f_name."','".$date_correct."','".$date_str."','".$id_region."','".$text."');\n";	
	}
    #save to csv file
	if(scalar(@date) > 0)
	{
		$data .= $id.";".$f_name.";".$date[0].";".$id_region.";".$text."\n";
	}
	else
	{
		$data .= $id.";".$f_name.";01.01.1900;".$id_region.";".$text.")\n";
	}
}
print OUTPUT_CSV $data;
close(OUTPUT_CSV);

print OUTPUT_SQL $sql;
close(OUTPUT_SQL);

#print LOGFILE $str_log;
#close(LOGFILE);

my %dico_report = Modules::Dico::LoadDicoCSV($FILE_REPORT_CSV);

#créer la table rélation
#table Plant_Disease_Nuisibilité (Id,Id_Report,Id_Plant, Id_Disease, Comment)
my $FILE_OUTPUT2 = $DIR_SQL."plant_disease.sql";
open(OUTPUT2,'>:raw:encoding(UTF8)',$FILE_OUTPUT2) || die "Can't open this file: $FILE_OUTPUT2";
my $data2 = "Delete from plant_disease;\n";
my $id2 = 0;
#créer la table rélation
#table Plant_Bioagresseur_Nuisibilité (Id,Id_Report,Id_Plant, Id_Bioagresor, Comment)
my $FILE_OUTPUT3 = $DIR_SQL."plant_bioagressor.sql";
open(OUTPUT3,'>:raw:encoding(UTF8)',$FILE_OUTPUT3) || die "Can't open this file: $FILE_OUTPUT3";
my $data3 = "Delete from plant_bioagressor;\n";
my $id3 = 0;
for my $v_bulletin (keys %dico_report)
{
	my $id_bulletin = $dico_report{$v_bulletin};
	#avec entitié :p
	my $req = lc($v_bulletin).":p:";
	my %hash_plant = ();
	if(exists $data_result{$req})
	{
		my @arr = @{$data_result{$req}};
		foreach(@arr)
		{
			$hash_plant{$_}++;
		}
	}
	#avec relation :p:m
	$req = lc($v_bulletin).":p:m:";
	#print "$req\n";
	if(exists $data_result{$req})
	{
		my @arr = @{$data_result{$req}};
		foreach(@arr)
		{
			$id2 += 1;
			my @element = split(":",$_);	
			my ($id_p,$id_m) = (0,0);
			#print $element[1]."\n";
			if(exists $dico_plant{$element[1]})
			{
				$id_p = $dico_plant{$element[1]};
				$hash_plant{$element[1]}--;
			}
			if(exists $dico_disease{$element[2]})
			{
				$id_m = $dico_disease{$element[2]};
			}
			else
			{
				print "Maladie n'existe pas: $req:$_ => ".$element[2]."\n";
			}
			#$data .= $id.";".$id_bulletin.";".$id_p.";".$id_m.";\n";
			$data2 .= "Insert into plant_disease(Id,Id_Report,Id_Plant, Id_Disease) values ('".$id2."','".$id_bulletin."','".$id_p."','".$id_m."');\n";
		}
	}
	#avec relation :p:m:n:
	$req = lc($v_bulletin).":p:m:n:";
	if(exists $data_result{$req})
	{
		my @arr = @{$data_result{$req}};
		foreach(@arr)
		{
			$id2 += 1;
			my @element = split(":",$_);
			my ($id_p,$id_m) = (0,0);
			#print $element[1]."\n";
			if(exists $dico_plant{$element[1]})
			{
				$id_p = $dico_plant{$element[1]};
				$hash_plant{$element[1]}--;
			}
			if(exists $dico_disease{$element[2]})
			{
				$id_m = $dico_disease{$element[2]};
			}
			else
			{
				print "Maladie n'existe pas $req:$_ => ".$element[2]."\n";
			}
			$element[3] =~ s/'/''/g;
			$data2 .= "Insert into plant_disease(Id,Id_Report,Id_Plant,Id_Disease,Comment) values ('".$id2."','".$id_bulletin."','".$id_p."','".$id_m."','$element[3]');\n";	
		}
	}
	#avec relation :p:b
	$req = lc($v_bulletin).":p:b:";
	if(exists $data_result{$req})
	{
		my @arr = @{$data_result{$req}};
		foreach(@arr)
		{
			$id3 += 1;
			my @element = split(":",$_);
			my ($id_p,$id_b) = (0,0);
			#print $element[1]."\n";
			if(exists $dico_plant{$element[1]})
			{
				$id_p = $dico_plant{$element[1]};
				$hash_plant{$element[1]}--;
			}
			if(exists $dico_bioagressor{$element[2]})
			{
				$id_b = $dico_bioagressor{$element[2]};
			}
			else
			{
				print "Bioagresseur n'existe pas $req:$_ => ".$element[2]."\n";
			}
			#$data .= $id.";".$id_bulletin.";".$id_p.";".$id_b.";\n";
			$data3 .= "Insert into plant_bioagressor(Id,Id_Report,Id_Plant, Id_Bioagressor) values ('".$id3."','".$id_bulletin."','".$id_p."','".$id_b."');\n";	
		}
	}
	#avec relation :p:b:n:
	$req = lc($v_bulletin).":p:b:n:";
	if(exists $data_result{$req})
	{
		my @arr = @{$data_result{$req}};
		foreach(@arr)
		{
			$id3 += 1;
			my @element = split(":",$_);
			my ($id_p,$id_b) = (0,0);
			#print $element[1]."\n";
			if(exists $dico_plant{$element[1]})
			{
				$id_p = $dico_plant{$element[1]};
				$hash_plant{$element[1]}--;
			}
			if(exists $dico_bioagressor{$element[2]})
			{
				$id_b = $dico_bioagressor{$element[2]};
			}
			else
			{
				print "Bioagresseur n'existe pas $req:$_ => ".$element[2]."\n";
			}
			#$data .= $id.";".$id_bulletin.";".$id_p.";".$id_b.";$element[3]\n";
			$element[3] =~ s/'/''/g;
			$data3 .= "Insert into plant_bioagressor(Id,Id_Report,Id_Plant,Id_Bioagressor,Comment) values ('".$id3."','".$id_bulletin."','".$id_p."','".$id_b."','$element[3]');\n";	
		}
	}
	#this time is last, if a plant doesn't have a relation
	if(Modules::Utils::SizeHash(%hash_plant) > 0)
	{
		my ($id_p,$id_m_b) = (0,0);
		for my $key_plant (keys %hash_plant)
		{
			$id2 += 1;
			$id3 += 1;
			#print $v_bulletin."\n";
			if($hash_plant{$key_plant} > 0)
			{
				$id_p = $dico_plant{$key_plant};
				$data2 .= "Insert into plant_disease(Id,Id_Report,Id_Plant, Id_Disease) values ('".$id2."','".$id_bulletin."','".$id_p."','".$id_m_b."');\n";
				$data3 .= "Insert into plant_bioagressor(Id,Id_Report,Id_Plant, Id_Bioagressor) values ('".$id3."','".$id_bulletin."','".$id_p."','".$id_m_b."');\n";
			}	
		}	 
	}
}
print OUTPUT2 $data2;
close(OUTPUT2);

print OUTPUT3 $data3;
close(OUTPUT3);
