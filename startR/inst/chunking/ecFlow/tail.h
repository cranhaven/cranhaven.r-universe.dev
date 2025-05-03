if [ "$(type -t _ecf_report_back_)" = function ] ; then
  _ecf_report_back_
fi
wait                      # wait for background process to stop
if [[ %BIDIRECTIONAL% == "TRUE" ]] ; then
  ecflow_client --complete  # Notify ecFlow of a normal end
fi
trap 0                    # Remove all traps
exit 0                    # End the shell
