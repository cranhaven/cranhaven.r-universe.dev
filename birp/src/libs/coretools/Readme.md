# README #

This repository contains various utility classes used by other projects.

# Usage #
This is a short overview of how to use this git repository as a submodule in existing projects.
More details about the usage of submodules in git can be found at: https://git-scm.com/book/en/v2/Git-Tools-Submodules

## Add submodule to existing project ##

To add this git project to an existing project, use the following command:

`git submodule add -b master https://bitbucket.org/WegmannLab/coretools.git`

## Use submodule ##

To use the submodule in a project it needs to be initialized in that project.
Use the following two commands in the root directory of the project

```
git submodule init
git submodule update --remote
```

Doing a git pull in the main project will not automatically download the latest version of the submodule.
To do so, enter the submodule folder (eg. `cd coretools`) and use git pull there.
Make sur you checkout the correct branch of the submodule when doing this, as by default the submodule will not follow any branch.

Alternatively the git pull command in the git root directory can be told to also pull all submodule, using the following parameter:

`git pull --recurse-submodules`

If you already use a branch that had a submodule added after you checked it out use:
```
git submodule update --init --recursive
```
To get the latest submodule.

To configure newer git versions to automatically pull submodules when using git pull, use:
```
git config --global submodule.recurse true
```

To get the newest remote version:
```
git submodule update --remote coretools/
```
If you then commit and push your changes, the link of your project to the submodule will be updated.

## Cloning a project with submodules 
To automatically clone also all submodules in a project when cloning it, add the `--recurse-submodules` to the `git clone` command like this:

`git clone --recurse-submodules git@bitbucket.org:phaentu/flink.git`

## Eclipse error: two main functions
Depending on your settings, it can happen that Eclipse throws an error that multiple main functions are available when compiling. To circumvent this, create a new configuration as follows: In Eclipse, right-click on your project folder and select Build Configurations -> Manage -> New. Add some name (e.g. UNITTESTING) and select "Existing Configurations". Press Ok. Now, you can exclude the files in the tests-folder from the Debug/Release Configuration (right-click on one of the files -> Resource Configurations -> Exclude from Build -> Select "Debug" and "Resource"). Additionally, exclude the file that contains your main function from your new testconfiguration (right-click on your main-file -> Resource Configurations -> Exclude from Build -> Select "UNITTESTING").
