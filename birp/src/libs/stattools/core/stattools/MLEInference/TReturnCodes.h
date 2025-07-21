//
// Created by madleina on 04.06.21.
//

#ifndef TRETURNCODES_H
#define TRETURNCODES_H

#include <cstdint>
#include <string>

namespace stattools {

enum ReasonForFailure : uint8_t {
	noFailure,
	invalidInput,
	reachMaxIterations,
	numericalIssues,
	spuriousConvergence,
	zeroNotEnclosed
};
enum CriticalPoint : uint8_t { noCriticalPoint, minimum, maximum, saddlePoint };

template<typename RESULTTYPE> class TReturnCode {
protected:
	// converged or failed?
	RESULTTYPE _result{};
	bool _converged                    = false;
	std::string _message               = "-";
	ReasonForFailure _reasonForFailure = noFailure;
	CriticalPoint _criticalPoint       = noCriticalPoint;
	double _valueAtResult              = 0.0;
	size_t _numIterations              = 0;

public:
	void setFailed(std::string_view Message, ReasonForFailure Reason, size_t NumIterations) {
		_converged        = false;
		_message          = Message;
		_reasonForFailure = Reason;
		_numIterations    = NumIterations;
	}

	void setFailed(std::string_view Message, ReasonForFailure Reason, const RESULTTYPE &Result, size_t NumIterations) {
		_converged        = false;
		_message          = Message;
		_reasonForFailure = Reason;
		_result           = Result;
		_numIterations    = NumIterations;
	}

	void setFailed(std::string_view Message, ReasonForFailure Reason, const RESULTTYPE &Result,
				   CriticalPoint CriticalPoint, double ValueAtResult, size_t NumIterations) {
		_converged        = false;
		_message          = Message;
		_reasonForFailure = Reason;
		_result           = Result;
		_criticalPoint    = CriticalPoint;
		_valueAtResult    = ValueAtResult;
		_numIterations    = NumIterations;
	}

	void setSucceeded(const RESULTTYPE &Result, CriticalPoint CriticalPoint, double ValueAtResult,
					  size_t NumIterations) {
		_converged     = true;
		_result        = Result;
		_criticalPoint = CriticalPoint;
		_valueAtResult = ValueAtResult;
		_numIterations = NumIterations;
	}

	void setSucceeded(const RESULTTYPE &Result, CriticalPoint CriticalPoint, double ValueAtResult, size_t NumIterations,
					  std::string_view Message) {
		_converged     = true;
		_result        = Result;
		_criticalPoint = CriticalPoint;
		_message       = Message;
		_valueAtResult = ValueAtResult;
		_numIterations = NumIterations;
	}

	[[nodiscard]] bool converged() const { return _converged; }

	[[nodiscard]] std::string_view message() const { return _message; }

	const RESULTTYPE &result() const { return _result; }

	double valueAtResult() const { return _valueAtResult; }

	[[nodiscard]] ReasonForFailure reason() const { return _reasonForFailure; }

	[[nodiscard]] CriticalPoint criticalPoint() const { return _criticalPoint; }

	[[nodiscard]] size_t numIterations() const { return _numIterations; }
};

};     // namespace stattools
#endif // TRETURNCODES_H
