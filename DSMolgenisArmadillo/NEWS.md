# DSMolgenisArmadillo 3.0.0
* Feat: Implemented functionality to refresh tokens if they have expired. This involved adding a 
new function `armadillo.get_credentials` which replaces `armadillo.get_token`, and also retrieves
a refresh token which is needed for the refresh. Functionality is added which works in the background
to check if the token has expired, and if so refresh it.

# DSMolgenisArmadillo 2.0.3

# DSMolgenisArmadillo 2.0.2

# DSMolgenisArmadillo 2.0.0

# DSMolgenisArmadillo 1.4.1

# DSMolgenisArmadillo 1.4.0

# DSMolgenisArmadillo 1.4.0
* Add profiles
* Update DSI to 1.3.0

# DSMolgenisArmadillo 1.3.7

* Fix: infinite loop is caused by not checking on FAILED state in async request

# DSMolgenisArmadillo 1.3.6

* Change owner to Sido Haakma

# DSMolgenisArmadillo 1.3.5

* Upgrade to DSI 1.2

# DSMolgenisArmadillo 1.3.4

* Fix #41: dsGetInfo should await result

# DSMolgenisArmadillo 1.3.3

# DSMolgenisArmadillo 1.3.2

* Added a `NEWS.md` file to track changes to the package.
