opendir $dir, "./" or die "Cannot open directory: $!";
@files = readdir $dir;
closedir $dir;
# @files = @files[2 .. $#files];

# $lines = 0;
for $file (@files){
	if($file =~ /\.R$/){
		print "$file\n";
		rename($file, $file . '.bak');
		open(IN, '<' . $file . '.bak') or die "Cannot open backup file to read: $!";
		open(OUT, '>' . $file) or die "Cannot open file to write: $!";
		while(<IN>){
			s/(identical\(.*\))\s*\#\s*(TRUE|FALSE).*$/message\('$1 \(should be $2\): ', $1\)/;
			s/(all.equal\(.*\))\s*\#\s*(TRUE|FALSE).*$/message\('$1 \(should be $2\): ', $1\)/;
			print OUT "$_";
			};
		# unlink($file.".bak") or die "Cannot delete file", $file.".bak", ": $!";
	}
	close IN;
	close OUT;
}
