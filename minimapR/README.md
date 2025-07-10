# Installation
git clone https://github.com/jake-bioinfo/minimapR.git

R CMD INSTALL minimapR

## System Requirements

This package requires [Git](https://git-scm.com/) to be installed on your system. Git is used for cloning repositories.

### Installing Git

- **Windows**: Download and install Git from [gitforwindows.org](https://gitforwindows.org/).
- **macOS**: Install Git via Homebrew with `brew install git`.
- **Linux**: Install Git using the package manager, e.g., `sudo apt-get install git` for Debian-based systems.

Ensure that Git is included in your system PATH so that it can be accessed from the command line.

This package is based on the work of Heng Li. It is a wrapper for a package written by him 'minimap2'
http://liheng.org/
https://github.com/lh3/minimap2

Citations

Li, H. (2018). Minimap2: pairwise alignment for nucleotide sequences. Bioinformatics, 34:3094-3100. doi:10.1093/bioinformatics/bty191

Li, H. (2021). New strategies to improve minimap2 alignment accuracy. Bioinformatics, 37:4572-4574. doi:10.1093/bioinformatics/btab705
