
#include "TTaskList.h"
#include "coretools/Strings/stringProperties.h"
#include "coretools/Strings/stringManipulations.h"

namespace coretools {

void TTaskList::_throwErrorUnknownTask(std::string_view origTask) const {
    // calculate Levenshtein Distance and return closes match
    std::vector<std::string> available;
    for (auto &it : _allTasks) {
        available.push_back(it.first);
    }	

    auto match = coretools::str::findClosestMatchLevenshtein<false>(origTask, available, 1);

    // report best unless distance is too high
    if (match.second < origTask.length() - 1){
        UERROR("Unknown task '", origTask, "'! Did you mean task '", match.first, "'?");
    } else {
        //if input string is not a task, it might be a file
        //search for possible existence of path in filename
        std::filesystem::path filePath = std::filesystem::current_path();
        std::string additionalPath;
        //if there is a path, add it to filepath
        if(origTask.find("/") != std::string::npos || origTask.find("\\") != std::string::npos){
            std::string tmp = str::toString(origTask);
            additionalPath = str::extractPath(tmp);
            std::filesystem::directory_entry direct(additionalPath);
            // check if path is a directory, if not don't save it
            if(direct.is_directory())
                filePath.append(additionalPath);
            else
                additionalPath.clear();
        }

        //loop over all files at path, set their names to lower case and calculate the distance to input string
        std::vector<std::string> filenames;
        for (auto const& dir_entry : std::filesystem::directory_iterator{filePath}){
            if (is_regular_file(dir_entry)){
                std::string fname = dir_entry.path().filename().generic_string();
                filenames.push_back(additionalPath + fname);
                std::transform(filenames.back().begin(), filenames.back().end(), filenames.back().begin(), ::tolower);
            }
        }
        auto fileMatch = coretools::str::findClosestMatchLevenshtein<false>(origTask, filenames, 0.5);
        
        // if lowest file distance is lower than lowest task distance, report that
        if (fileMatch.second < match.second){ 
            UERROR("Unknown task or file '", origTask, "'! Did you mean file '", additionalPath, fileMatch.first, "'?");
        } else {
            UERROR("Unknown task or file '", origTask, "'!");
        }
    }
}

} // namespace coretools
