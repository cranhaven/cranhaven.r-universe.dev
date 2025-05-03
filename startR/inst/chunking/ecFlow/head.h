#!/bin/bash

if [[ %REPORT_BACK% == "TRUE" ]] ; then
  if [[ %BIDIRECTIONAL% == "TRUE" ]] ; then
    _ecf_report_back_() {
      rsync -rav %REMOTE_ECF_HOME%/%SUITE%/%FAMILY%/ %EC_HOST_FULL%:%ECF_HOME%/%SUITE%/%FAMILY%/
      if [[ $(ls %REMOTE_ECF_HOME%/%RESULT_FILE_ID% 2>/dev/null | wc -l) -ge 1 ]] ; then
        scp %REMOTE_ECF_HOME%/%RESULT_FILE_ID% %EC_HOST_FULL%:%ECF_HOME%/
        rm -f %REMOTE_ECF_HOME%/%RESULT_FILE_ID%
      fi
    }
  fi
fi

set -e # stop the shell on first error
set -u # fail when using an undefined variable

#module load ecFlow/%ECF_VERSION%-foss-2015a
set -x # echo script lines as they are executed

if [[ %BIDIRECTIONAL% == "TRUE" ]] ; then
  # Defines the variables that are needed for any communication with ECF
  export ECF_PORT=%ECF_PORT%    # The server port number
  export ECF_HOST=%EC_HOST_FULL%    # The host name where the server is running
  export ECF_NAME=%ECF_NAME%    # The name of this current task
  export ECF_PASS=%ECF_PASS%    # A unique password
  export ECF_TRYNO=%ECF_TRYNO%  # Current try number of the task
  export ECF_RID=$$             # record the process id. Also used for zombie detection

  # Define the path where to find ecflow_client
  # make sure client and server use the *same* version.
  # Important when there are multiple versions of ecFlow
  export PATH=/usr/local/bin:$PATH

  # Tell ecFlow we have started
  ecflow_client --init=$$
else
  running_file=$(echo "%RESULT_FILE_ID%" | sed -e 's/*_/x_/g' | sed -e 's/*/.running/g')
  touch %REMOTE_ECF_HOME%/$running_file
fi

# Define a error handler
ERROR() {
   if [[ %BIDIRECTIONAL% == "FALSE" ]] ; then
     err_file=$(echo "%RESULT_FILE_ID%" | sed -e 's/*_/x_/g' | sed -e 's/*/.crashed/g')
     touch %REMOTE_ECF_HOME%/$err_file
   fi
   if [ "$(type -t _ecf_report_back_)" = function ] ; then
     _ecf_report_back_
   fi
   set +e                      # Clear -e flag, so we don't fail
   wait                        # wait for background process to stop
   if [[ %BIDIRECTIONAL% == "TRUE" ]] ; then
     ecflow_client --abort=trap  # Notify ecFlow that something went wrong
   fi
   trap 0                      # Remove the trap
   exit 0                      # End the script
}


# Trap any calls to exit and errors caught by the -e flag
trap ERROR 0


# Trap any signal that may cause the script to fail
trap '{ echo "Killed by a signal"; ERROR ; }' 1 2 3 4 5 6 7 8 10 12 13 15
