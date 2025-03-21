/* BugsHelper - Perform one or more OpenBUGS API commands as specified
in the command line arguments.

ARGUMENTS (not named)

	argv[1]: Full path to temporary directory containing the buffer file
	for messages from the main API commands, and any input and output
	corresponding to the commands.

	argv[2]: Full path to temporary directory containing the buffer file
	for messages from the Internalize and Externalize commands.  We do not
	want these to overwrite the main buffer.

	argv[3]: Name of the file used to store the externalized model
	state. This will be saved in the directory specified in argv[1].

	argv[4]: Name of the command file, which contains a list of
	calls to OpenBUGS API functions and the types of each call.
	These are given on alternate lines of this file, in the order:

	cmd, cmdtype, cmd, cmdtype, ...

	where "cmd" is an OpenBUGS API command, and "cmdtype" is an integer
	specifying the API function being called.  Currently allowed values of
	"cmdtype" are

	0 : if "cmd" is a call to the "CmdInterpreter" API function
	1 : if "cmd" is a call to the "Integer" API function
	2 : if "cmd" is a call to the "CharArray" API function
	3 : if "cmd" is a call to the "RealArray" API function
	4 : if "cmd" is a call to "BugsCmd"  (currently unused in BRugs, and not tested)

	argv[5]: Number of commands contained in the command file.

INPUT AND OUTPUT

	Some OpenBUGS commands require input or produce output.  The input and
	output for the Nth command given in the call to BugsHelper are stored
	in files called inputN.txt and outputN.txt, and are saved in the
	directory specified in argv[1].


EXAMPLE

	The following command checks a BUGS model file stored in the file
	/path/to/Examples/Ratsmodel.txt and then loads a data file from
	/path/to/Examples/Ratsdata.txt.  Temporary files are stored in
	subdirectories of /tmp.

	"BugsEmbed.SetFilePath('/scratch/chris/lib/R/BRugs/OpenBUGS/Examples/Ratsdata.txt');BugsEmbed.LoadDataGuard;BugsEmbed.LoadData" 0

	/path/to/BugsHelper "/tmp/RtmpaRQois" "/tmp/RtmpaRQois/trash" "file327b23c6.bug" "BugsEmbed.SetFilePath('/path/to/Examples/Ratsmodel.txt');BugsEmbed.ParseGuard;BugsEmbed.Parse" 0 "BugsEmbed.SetFilePath('/path/to/Examples/Ratsdata.txt');BugsEmbed.LoadDataGuard;BugsEmbed.LoadData" 0


TODO
        How to handle an OpenBUGS "trap"?  If the library call terminates with a crash, then BugsHelper terminates with an error code of 1.
	But this error code is also used for "unknown module" internal errors which are handled.  How to distinguish?


AUTHOR

	Chris Jackson <chris.jackson@mrc-bsu.cam.ac.uk>

 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <assert.h>

#define NODEBUG

/* OpenBUGS API functions from libOpenBUGS.so */
#include <errno.h>
extern void CLI (void);
extern void BugsCmd(char **command, int *len);
extern void CharArray (char **procedure, int *len, char **x, int *lenX, int *res);
extern void CmdInterpreter (char **command, int *len, int *res);
extern void Guard (char **procedure, int *len, int *x, int *res);
extern void Integer (char **procedure, int *len, int *x, int *res);
extern void IntegerArray (char **procedure, int *len, int *x, int *lenX, int *res);
extern void Real (char **procedure, int *len, double *x, double *y, int *res);
extern void RealArray (char **procedure, int *len, double *x, int *lenX, int *res);
extern void SetWorkingDir (char **path, int *len);
extern void SetTempDir(char **path, int *len);
extern void UseBufferFile (void);
extern void UseConsole (void);

void read_input_real(char *tmpdir, double **out, int *len, int cmdno) {
  char *fname;
  struct stat buf={.st_dev = 0};
  FILE *ifp;
  double tmp;
  size_t sz = strlen(tmpdir) + 16;
  fname = (char *) malloc(sz);
  if(fname == NULL) {
      fprintf(stderr, "allocation failed in BugsHelper");
      exit(1);
  }
  snprintf(fname, sz, "%s/input%d.txt", tmpdir, cmdno);
  if (stat(fname, &buf) != -1) {
    *len = 0;
    ifp = fopen(fname, "r");
    while (fscanf(ifp, "%lf", &tmp) == 1)
      ++*len;
    *out = (double *) malloc(sizeof(double)*(*len));
#ifdef DEBUG
    printf("Reading %d values from %s\n", *len, fname);
#endif
    fseek(ifp, 0, SEEK_SET); /* move to start of file */
    *len = 0;
    while (fscanf(ifp, "%lf", &((*out)[*len])) == 1){
      ++*len;
    }
    fclose(ifp);
  }
  free(fname);
}

void read_input_char(char *tmpdir, char **out, int *len, int cmdno) {
  char *fname;
  struct stat buf={.st_dev = 0};
  FILE *ifp;
  size_t sz = strlen(tmpdir) + 16;
  fname = (char *) malloc(strlen(tmpdir) + 16);
  if(fname == NULL) {
      fprintf(stderr, "allocation failed in BugsHelper");
      exit(1);
  }
  snprintf(fname, sz, "%s/input%d.txt", tmpdir, cmdno);
  if (stat(fname, &buf) != -1) {
    *len = buf.st_size;
    *out = (char *) malloc(*len + 1);
    ifp = fopen(fname, "r");
#ifdef DEBUG
    printf("Reading %d characters from %s\n", *len, fname);
#endif
    *len = 0;
    while(((*out)[*len] = getc(ifp)) != EOF) {
      ++*len;
    }
    (*out)[*len] = '\0';
    fclose(ifp);
  }
  free(fname);
}

void write_output_int(char *tmpdir, int out, int cmdno) {
  char *fname;
  FILE *ofp;
  size_t sz = strlen(tmpdir) + 16;
  fname = (char *) malloc(sz);
  if(fname == NULL) {
      fprintf(stderr, "allocation failed in BugsHelper");
      exit(1);
  }
  snprintf(fname, sz, "%s/output%d.txt", tmpdir, cmdno);
  ofp = fopen(fname, "w");
#ifdef DEBUG
  printf("Writing integer %d to %s\n", out, fname);
#endif
  fprintf(ofp, "%d", out);
  fclose(ofp);
  free(fname);
}

void write_output_real(char *tmpdir, double *out, int len, int cmdno) {
  char *fname;
  FILE *ofp;
  int i;
  unsigned int sz = strlen(tmpdir) + 16;
  fname = (char *) malloc(sz);
  if(fname == NULL) {
      fprintf(stderr, "allocation failed in BugsHelper");
      exit(1);
  }
  snprintf(fname, sz, "%s/output%d.txt", tmpdir, cmdno);
  ofp = fopen(fname, "w");
#ifdef DEBUG
  printf("Writing %d reals to %s\n", len, fname);
#endif
  for (i=0; i<len; ++i)
    fprintf(ofp, "%lf ", out[i]);   // TODO is precision acceptable?
  fclose(ofp);
  free(fname);
}

void write_output_char(char *tmpdir, char *out, int cmdno) {
  char *fname;
  FILE *ofp;
  size_t sz = strlen(tmpdir) + 16;
  fname = (char *) malloc(sz);
  if(fname == NULL) {
      fprintf(stderr, "allocation failed in BugsHelper");
      exit(1);
  }
  snprintf(fname, sz, "%s/output%d.txt", tmpdir, cmdno);
#ifdef DEBUG
  printf("Writing string %s to %s\n", out, fname);
#endif
  ofp = fopen(fname, "w");
  fprintf(ofp, "%s", out);
  fclose(ofp);
  free(fname);
}

void do_TempDir(char *dir) {
  int length;
  length = strlen(dir);
  SetTempDir(&dir, &length);
}

int do_Cmd(char *cmd) {
  int length, res=0;
  length = strlen(cmd);
#ifdef DEBUG
  printf("%s: %d\n", cmd, strlen(cmd));
#endif
  CmdInterpreter(&cmd, &length, &res);
  return res;
}

int do_Integer(char *cmd, char *tmpdir, int cmdno) {
  int length, out, res=0;
  // doesn't need input -- assume only used for reading integers
  length = strlen(cmd);
#ifdef DEBUG
  printf("%s: %d\n", cmd, strlen(cmd));
#endif
  Integer(&cmd, &length, &out, &res);
  write_output_int(tmpdir, out, cmdno);
  return res;
}

int do_CharArray(char *cmd, char *tmpdir, int cmdno) {
  int length, outlength, res=0;
  char *out;
  length = strlen(cmd);
  read_input_char(tmpdir, &out, &outlength, cmdno);
#ifdef DEBUG
  printf("%s: args=%s, arglength=%d\n", cmd, out, outlength);
#endif
  CharArray(&cmd, &length, &out, &outlength, &res);
  write_output_char(tmpdir, out, cmdno);
  free(out);
  return res;
}

int do_RealArray(char *cmd, char *tmpdir, int cmdno) {
  int length, outlength, res=0;
#ifdef DEBUG
  int i;
#endif
  double *out;
  read_input_real(tmpdir, &out, &outlength, cmdno);
  length = strlen(cmd);
#ifdef DEBUG
  printf("%s: arglength=%d\n", cmd, outlength);
  for (i=0; i<outlength; ++i) printf("%lf ",out[i]);
  printf("\n");
#endif
  RealArray(&cmd, &length, out, &outlength, &res);
  write_output_real(tmpdir, out, outlength, cmdno);
  free(out);
  return res;
}

void do_BugsCmd(char *cmd) {
  int length;
  length = strlen(cmd);
#ifdef DEBUG
  printf("%s: %d\n", cmd, strlen(cmd));
#endif
  BugsCmd(&cmd, &length);
}

int do_Internalize(char *tmpdir, char *extfile){
  char *extpath, *int_cmd;
  struct stat buf={.st_dev = 0};
  int res;
  size_t sz = strlen(tmpdir) + 2 + strlen(extfile);
  extpath = (char *) malloc(sz);
  if(extpath == NULL) {
      fprintf(stderr, "allocation failed in BugsHelper");
      exit(1);
  }
  snprintf(extpath, sz, "%s/%s", tmpdir, extfile);
  if (stat(extpath, &buf) == 0) { /* if file exists */
    sz = strlen(tmpdir) + strlen(extfile) + 66;
    int_cmd = (char *) malloc(sz);
    snprintf(int_cmd, sz, "BugsEmbed.InternalizeModel(\'%s/%s\')",
	     tmpdir, extfile);
    res = do_Cmd(int_cmd);
    free(int_cmd);
  }
  else res = 10; /* TODO handle error */
  free(extpath);
  return res;
}

int do_Externalize(char *tmpdir, char *extfile){
  char *ext_cmd;
  int res;
  size_t sz = strlen(tmpdir) + strlen(extfile) + 66;
  ext_cmd = (char *) malloc(sz);
  if(ext_cmd == NULL) {
      fprintf(stderr, "allocation failed in BugsHelper");
      exit(1);
  }
  snprintf(ext_cmd, sz, "BugsEmbed.ExternalizeModel(\'%s/%s\')",
	   tmpdir, extfile);
  res = do_Cmd(ext_cmd);
  free(ext_cmd);
  return res;
}

int main(int argc, char **argv){
#ifdef DEBUG
    printf("Entering BUGSHelper...\n");
#endif
    int i, res=0, ncmds, len;
  char *tmpdir = argv[1];
  char *trashdir = argv[2];
  char *extfile = argv[3];
  char *cmdfile = argv[4]; 
  char *cmd, cmd_type;
  char *output_to_buffer_cmd = "BugsMappers.SetDest(2)";
  FILE *ifp;
  struct stat buf={.st_dev = 0};
  do_Cmd(output_to_buffer_cmd);
  do_TempDir(trashdir);
  do_Internalize(tmpdir, extfile);
  do_TempDir(tmpdir);

// Commands and command type IDs are read from alternate lines of command file
  ifp = fopen(cmdfile, "r");
  assert(stat(cmdfile, &buf) != -1);
  len = buf.st_size;
  cmd = (char *) malloc(len + 1); // command cannot be bigger than the command file
  sscanf(argv[5], "%d", &ncmds);

  for (i=0; i<ncmds; ++i) {
    len = 0;
    while((cmd[len] = getc(ifp)) != '\n') {
      ++len;
    }
    cmd[len] = '\0';
    cmd_type = getc(ifp);
    assert(getc(ifp) == '\n');

#ifdef DEBUG
    printf("%s, type %c of %d cmds\n", cmd, cmd_type, ncmds);
#endif
    switch(cmd_type){
    case '0':
      res = do_Cmd(cmd);
      break;
    case '1':
      res = do_Integer(cmd, tmpdir, i+1);
      break;
    case '2':
      res = do_CharArray(cmd, tmpdir, i+1);
      break;
    case '3':
      res = do_RealArray(cmd, tmpdir, i+1);
      break;
    case '4':
      do_BugsCmd(cmd);
      break;
    default:
      printf("Unsensible input: command type %c\n", cmd_type);
      break;
    }
    if (res > 0) return res;
  }
  free(cmd);
  do_TempDir(trashdir);
  do_Externalize(tmpdir, extfile);
  return res;
}
