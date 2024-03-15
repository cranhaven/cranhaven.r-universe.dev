#ifndef TRACKEXPRESSIONITERATORBASE_H_
#define TRACKEXPRESSIONITERATORBASE_H_

//------------------------------ TrackExpressionIteratorBase -------------------------------------

class TrackExpressionIteratorBase {
public:
	enum Type { FIXED_BIN, INTERVALS1D, INTERVALS2D, NUM_TYPES };

	static const char *TYPE_NAMES[NUM_TYPES];

	TrackExpressionIteratorBase(Type type) : m_type(type), m_isend(true) {}
	virtual ~TrackExpressionIteratorBase() {}

	Type get_type() const { return m_type; }

	bool is_1d() const { return m_type == FIXED_BIN || m_type == INTERVALS1D; }
	bool is_2d() const { return m_type == INTERVALS2D; }

	// returns false if end reached
	virtual bool next() = 0;

	bool isend() const { return m_isend; }

protected:
	Type        m_type;
	bool        m_isend;
};

#endif /* TRACKEXPRESSIONITERATORBASE_H_ */
