
#!/bin/bash

echo -e "checking for GCC version..."
version=$(gcc --version | awk 'NR==1 {print $NF}')
echo "GCC version: $version"
versionMajor=$(echo $version | cut -d. -f1)
if [ $versionMajor -gt 7 ]; then echo "GCC version is sufficient for birp"; else echo "ERR: please use GCC version 8 or higher."; exit 1 ;fi


cmake=cmake
update=FALSE

# define he help
usage='
\n \******************************COMPILING BIRP*****************************************\
\n
\n\t This script compiles the birp executable.
\n\t Usage: bash compile_birp.sh [options]
\n\t 
\n\t 
\n\t Optional parameters:
\n\t  -h \t open this page
\n\t 
\n\t  -c \t specify the full path to a cmake executable. 
\n\t     \t Required version: 2.14 or higher
\n\t     \t Default: cmake
\n\t 
\n\t  -a \t specify the full path to a locally installed Armadillo library.
\n\t 
\n\t  -u \t update birp to the newest version
\n\t 
\n \**************************************************************************************\
'

while getopts ":hc:a:u" OPTION
do
    case $OPTION in
    h)
      echo -e $usage
	    exit 1
	    ;;
    c)
      cmake=${OPTARG}
      if [[ -f $cmake ]]; then
        echo -e "Using custom cmake version: $cmake \n"
      else
        echo -e "ERROR: File $cmake specified with -c does not exist. Please specify the full path to a cmake executable \n"
        echo -e $usage
        exit 1
      fi
		;;
    a)
      arm=${OPTARG}
      if [[ -d "$arm" ]]; then
        echo -e "Using custom armadillo version from: $arm \n"
      else
        echo -e "ERROR: Directory $arm specified with -a does not exist. Please specify the full path to the directory of your armadillo library \n" 
        echo -e $usage
        exit 1
      fi
      ;;
    u)
      update=TRUE
      echo "Will update birp to the newest version"
    ;;
    \?)
      echo -e "ERROR: no valid parameters defined! \n\n"
      echo -e $usage
      exit 1
      ;;
    :)
      echo "option requires an argument -- $OPTARG" 
      echo -e $usage
      exit 1
      ;;
    esac
done

echo -e "checking for cmake version..."
version=$($cmake --version | awk 'NR==1 {print $NF}')
echo "cmake version: $version"
version1=$(echo $version | cut -d. -f1)
version2=$(echo $version | cut -d. -f2)
if ( [ $version1 == 2 ] && [ $version2 -gt 13 ]) || [ $version1 -gt 2 ] ; then echo "Cmake version is sufficient for birp"; else echo "ERR: please use cmake version 2.14 or higher."; exit 1 ;fi



if [[ $update == "TRUE" ]]; then
  echo -e "updating birp ...."
  # git clean -df ##if you update from a much older version and cannot properly compile, try uncommenting this line
  git pull 
  echo -e "done updating"
fi

mkdir -p build
cd build

echo -e "compiling...."
if [ -z $arm ]; then
  $cmake ../
  make
else
  $cmake -DCMAKE_PREFIX_PATH="${CMAKE_PREFIX_PATH};$arm"
  make
fi

echo -e "Done. The birp executable is located in build/birp. \n"

cd ..
