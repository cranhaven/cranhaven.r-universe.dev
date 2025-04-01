


# Vignettes and tests

Tests and vignettes need an empty ElasticSearch (ES) instance to really test the core KibioR methods (e.g. push, pull, search). They can be conducted throught an ES instance run via Docker to easily select multiple versions.
Unfortunately, interactive build has been removed since each build should test them on the spot (with the current machine they are tested on). 

When linked to ES, you can run the **full test suite** and compile the **interactive vignettes**.
Follow the rest of the file to make it happen.

## Folder organization

After downloading the project sources, you can find all that is needed inside the "doc_env" folder:

- "test_suite" folder contains all tests.
- "interactive_vignettes" contains all interactive vignettes.
- "kibior_build.R" is a helper that will automatically be called when trying to build/check tests and vignettes.

## Requirements

1. An empty and accessible ES running instance.
You can download the project from ES website or run a single instance via Docker.
See the fixed vignette, section **Deploying an Elasticsearch instance** to set one with Docker.

2. The Github version of the project. The packaged version has not some scripts that automatically load environment variables and prepare for tests. The scripts required are: `inst/doc_env/kibior_build.R` and `inst/doc_env/tests_suite/helper.R`.

## Process

- Download `KibioR` source from Github.

### Install and check ES instance 

- Verify the accessibility of the ES instance:
  - Run `curl -XGET "<ES_ADDRESS>:<ES_PORT>"`
  - You should get a JSON result ending with `"You Know, For Search"`
- Set env variable to target the ES instance:
  - variables are:
    - `KIBIOR_BUILD_ES_ENDPOINT="elasticsearch"` for ES address (no default)
    - `KIBIOR_BUILD_ES_PORT=9200` for ES port (default, 9200)
    - `KIBIOR_BUILD_ES_USERNAME` for ES username (default, unused if not set)
    - `KIBIOR_BUILD_ES_PASSWORD` for ES password (default, unused if not set)
  - the most simple:
    - Put them in a `.Renviron` in the project root (one line for each).
    - They will be called automatically with the build/check steps.
- Test the setting with:
  - if you do not have a password: `curl -XGET "KIBIOR_BUILD_ES_ENDPOINT:KIBIOR_BUILD_ES_PORT"`
  - if you do: `curl -u KIBIOR_BUILD_ES_USERNAME:KIBIOR_BUILD_ES_PASSWORD -XGET "KIBIOR_BUILD_ES_ENDPOINT:KIBIOR_BUILD_ES_PORT"`

### Interactive vignettes generation

- Inside the project folder, take the "inst/doc_env/interactive_vignettes" folder content and copy it in "vignettes" folder.
- Go to the parent folder and run the build on Kibior folder: `R CMD build kibior`.
- Open the generated archive (something linke `kibior_<VERSION>.tar.gz`) to find the "introduction.nb.html"

### Full test suite

> Be warned that there are lots of tests (> 1500), a 1/3 are pushing/pulling data which takes time.
> My laptop is running all tests on a Dell inspiron core i5-8250U CPU @ 1.60GHz, 4c/8t.
> It takes about 45min to run all tests, but can easily take more than an hour to run on older CPUs.
> You can regulate the number of tests by modifying the calls inside the "tests/testthat.R" file.

- Inside the project folder, take the "inst/doc_env/test_suite" folder content and copy it in "tests" folder.
- 2 options from now:
  - In a terminal: go to the parent folder and run the build/check on Kibior folder: `R CMD build kibior`, then `R CMD check kibior_<VERSION>.tar.gz`.
  - In an R session: use `devtools` to run tests: `devtools::test()`.



# Data informations


- `test.bam.bai` file generated with

```
samtools view -h ftp://ftp.1000genomes.ebi.ac.uk/vol1/ftp/phase1/data/HG00154/alignment/HG00154.mapped.ILLUMINA.bwa.GBR.low_coverage.20101123.bam 17:7512445-7513455 -O bam > test.bam.bai
```


- `cpg.bed` file downloaded from

```
https://s3.amazonaws.com/bedtools-tutorials/web/cpg.bed
```


- `chr_y.gff3.gz` file downloaded from

```
ftp://ftp.ensembl.org/pub/release-99/gff3/homo_sapiens/Homo_sapiens.GRCh38.99.chromosome.Y.gff3.gz
```


- `dna_human_y.fa.gz` dna file downloaded and subsetted from 

```
ftp://ftp.ensembl.org/pub/release-99/fasta/homo_sapiens/dna/Homo_sapiens.GRCh38.dna_rm.chromosome.Y.fa.gz
```

- `ncrna_mus_musculus.fa.gz` rna file downloaded and subsetted from 

```
ftp://ftp.ensembl.org/pub/release-99/fasta/mus_musculus/ncrna/Mus_musculus.GRCm38.ncrna.fa.gz
```

- `pep_mus_spretus.fa.gz` aa file downloaded and subsetted from 

```
ftp://ftp.ensembl.org/pub/release-99/fasta/mus_spretus/pep/Mus_spretus.SPRET_EiJ_v1.pep.all.fa.gz
```
