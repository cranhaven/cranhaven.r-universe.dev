/*
 * main.cpp
 */

#include "TBirpCore.h"
#include "coretools/Main/TMain.h"

//---------------------------------------------------------------------------
// Existing Tasks
//---------------------------------------------------------------------------

void addTaks(coretools::TMain &main) {
	main.addRegularTask("simulate", new TTask_simulate());
	main.addRegularTask("infer", new TTask_infer());
}

//---------------------------------------------------------------------------
// Existing Integration tests
//---------------------------------------------------------------------------

void addTests(coretools::TMain &) {}

//---------------------------------------------------------------------------
// Main function
//---------------------------------------------------------------------------

int main(int argc, char *argv[]) {
	// Create main by providing a program name, a version, an
	// affiliation, link to repo and contact email
	coretools::TMain main("Birp", "0.1", "University of Fribourg", "https://bitbucket.org/wegmannlab/birp_cpp",
	                      "daniel.wegmann@unifr.ch");

	// add existing tasks and tests
	addTaks(main);
	addTests(main);

	// now run program
	return main.run(argc, argv);
}
