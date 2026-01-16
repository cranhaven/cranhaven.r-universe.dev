// util/SortedList.h
#ifndef ALUN_UTIL_SORTEDLIST_H
#define ALUN_UTIL_SORTEDLIST_H

#include "List.h"

namespace util{
class SortedList : public List
{
public:
	virtual int carefulCompare(Object *k, Object *p)
	{
		if (k == 0 && p == 0)
			return 0;

		if (p == 0)
			return 1;

		if (k == 0)
			return -1;

		return k->compare(p);
	}

	virtual void append(Object *k) override
	{
		ListLink *prev = tail;
		for ( ; prev != 0 && carefulCompare(k,prev->key) < 0; )
			prev = prev->prev;

		ListLink *next = ( prev == 0 ? head : prev->next );
		ListLink *l = new ListLink(k);

		if (prev != 0)
		{
			prev->next = l;
			l->prev = prev;
		}
		else
		{
			head = l;
		}

		if (next != 0)
		{
			next->prev = l;
			l->next = next;
		}
		else
		{
			tail = l;
		}
	}

	virtual void prepend(Object *k) override
	{
		ListLink *next = head;
		for ( ; next != 0 && carefulCompare(k,next->key) >= 0; )
			next = next->next;

		ListLink *prev = ( next == 0 ? tail : next->prev );
		ListLink *l = new ListLink(k);

		if (next != 0)
		{
			next->prev = l;
			l->next = next;
		}
		else
		{
			tail = l;
		}

		if (prev != 0)
		{
			prev->next = l;
			l->prev = prev;
		}
		else
		{
			head = l;
		}
	}

	std::string className() const override
	{
		return "SortedList";
	}
};
} // namespace util
#endif // ALUN_UTIL_SORTEDLIST_H
