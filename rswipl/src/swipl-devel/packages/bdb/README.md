# Interface to Berkeley DB

Berkeley DB is an embedded  database   engine,  originally  developed by
Sleepycat Software and now owned  by   Oracle.  The  database provides a
zero-configuration key-value store that can  be configured for different
levels of access concurrency, crash recovery support and transactions.

The current version is distributed  under   the  quite  restrictive AGPL
license with the option to buy a commercial license from Oracle.

## Installation

The interface works  with  a  wide   range  Berkeley  DB  versions. Some
installation notes:

  - Linux
  Simply install the development package for the desired libdb
  version.
  - MacOS
  Macports provides packages _dbNN_, the latest is `db60`. This
  installs the library in `/opt/local/lib/db60` and the headers in
  `/opt/local/include/db60`.  These paths are added to `LIBRARY_PATH`
  and `CPATH` in `build.templ`
  - Windows
  Currently we compiled our own version using MinGW.  This version
  does not yet support _replication_ and we are unsure about thread
  support.  This needs to be investigated.
