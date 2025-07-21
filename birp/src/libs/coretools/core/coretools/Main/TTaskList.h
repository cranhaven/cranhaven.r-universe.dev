/*
 * TTaskList.h
 *
 *  Created on: Mar 31, 2019
 *      Author: phaentu
 */

#ifndef TTASKLIST_H_
#define TTASKLIST_H_

#include <cctype>
#include <memory>

#include "coretools/Main/TLog.h"
#include "coretools/Main/TParameters.h"
#include "coretools/Main/TTask.h"
#include "coretools/Main/TError.h"

namespace coretools {

//---------------------------------------------------------------------------
// TTaskList
//---------------------------------------------------------------------------
class TTaskList {
private:
	std::map<std::string, std::unique_ptr<TTask>, std::less<>> _allTasks;
	std::vector<std::map<std::string, TTask *, std::less<>>> _regularTasks;
	std::map<std::string, TTask *, std::less<>> _debugTasks;
	std::vector<std::string> _generalCitations;
	std::vector<std::string> _groups;

	TTask *_getTask(std::string_view Task) {
		auto it = _allTasks.find(Task);
		if (it == _allTasks.end()) {
			instances::logfile().setVerboseLevel(VerboseLevel::verbose);
			printAvailableTasks();
			_throwErrorUnknownTask(Task);
		}
		return it->second.get();
	}

	void _throwErrorUnknownTask(std::string_view OrigTask) const;

public:
	TTaskList() {
		// First group is empty
		_groups.push_back("");
		_regularTasks.emplace_back();
	}
	void addGeneralCitation(std::string_view Citation) { _generalCitations.emplace_back(Citation); }

	void addRegularTask(std::string_view Name, TTask *Task) {
		auto p = _allTasks.emplace(Name, Task);
		_regularTasks.front().emplace(Name, p.first->second.get());
	}

	void addGroupedTask(std::string_view Group, std::string_view Name, TTask *Task) {
		DEBUG_ASSERT(_groups.size() == _regularTasks.size());
		auto p         = _allTasks.emplace(Name, Task);
		const size_t i = std::distance(_groups.begin(), std::find(_groups.begin(), _groups.end(), Group));
		if (i == _groups.size()) {
			_groups.emplace_back(Group);
			_regularTasks.emplace_back();
		}
		_regularTasks[i].emplace(Name, p.first->second.get());
	}

	void addDebugTask(std::string_view Name, TTask *Task) {
		auto p = _allTasks.emplace(Name, Task);
		_debugTasks.emplace(Name, p.first->second.get());
	}

	bool taskExists(std::string_view Task) const {
		return _allTasks.find(Task) != _allTasks.end();
	}

	void run() {
		run(instances::parameters().get("task"));
	}

	void run(std::string_view taskName) {
		// check if task exists
		TTask *task = _getTask(taskName);

		// make sure it contains general citations to be listed
		for (auto &c : _generalCitations) { task->addCitation(c); }

		// now run task
		task->run(taskName);
	}

	void printAvailableTasks() const {
		DEBUG_ASSERT(_groups.size() == _regularTasks.size());
		using instances::logfile;

		size_t maxLen = 0;
		struct TEntry {
			std::string_view name;
			std::string_view expl;
			TEntry(std::string_view Name, std::string_view Expl) : name(Name), expl(Expl) {};
		};

		std::vector<std::vector<TEntry>> tasks;
		tasks.resize(_groups.size());
		for (size_t i = 0; i < _groups.size(); ++i) {
			for (auto &it : _regularTasks[i]) {
				tasks[i].emplace_back(it.first, it.second->explanation());
				maxLen = std::max(maxLen, tasks[i].back().name.length());
			}
			std::sort(tasks[i].begin(), tasks[i].end(), [](const auto &s1, const auto &s2) {
				const auto N = std::min(s1.name.size(), s2.name.size());
				for (size_t i = 0; i < N; ++i) {
					if (toupper(s1.name[i]) == toupper(s2.name[i])) continue;
					return toupper(s1.name[i]) < toupper(s2.name[i]);
				}
				return s1.name.size() < s2.name.size();
			});
		}

		logfile().setVerboseLevel(VerboseLevel::verbose);
		logfile().startIndent("Available tasks:");
		for (size_t i = 0; i < _groups.size(); ++i) {
			// now print all tasks
			if (! _groups[i].empty()) {
				logfile().startIndent(_groups[i], " tasks:");
			} else {
				logfile().addIndent();
			}
			for (const auto task : tasks[i]) {
				std::string s{task.name};
				s.append(maxLen - task.name.length() + 3, ' '); // add white spaces to align explanations
				logfile().list(s, task.expl);
			}
			logfile().endIndent();
			logfile().write();
		}
		logfile().endIndent();
	}
};

} // namespace coretools

#endif /* TTASKLIST_H_ */
