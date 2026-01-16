// util/List.h
#ifndef ALUN_UTIL_LIST_H
#define ALUN_UTIL_LIST_H

#include "Object.h"
#include "Random.h"

namespace util{
class ListLink : public Object
{
friend class List;
friend class SortedList;

private:
	Object *key;
	ListLink *next;
	ListLink *prev;

	ListLink(Object *k);

public:
	inline Object* getKey()
	{
		return key;
	}

	std::string className() const override;
	void write(std::ostream &os) const override;
};

class List : public Object
{
protected:
	ListLink *head;
	ListLink *tail;

private:
	ListLink *current;

	inline ListLink *listget(Object *o)
	{
		for (ListLink *l = head; l != 0; l = l->next)
			if (l->key == o)
				return l;
		return 0;
	}

public:
	List();
	~List();

	inline void clear()
	{
		for (ListLink *l = head; l != 0; )
		{
			ListLink *ll = l;
			l = l->next;
			delete ll;
		}
		head = 0;
		tail = 0;
		current = 0;
	}

	inline List *copy()
	{
		List *l = new List();
		for (init(); hasNext(); )
			l->append(next());
		return l;
	}

	Object * random(Random *r) const;
	virtual void append(Object *k);
	virtual void prepend(Object *k);
	inline Object *remove(Object *k)
	{
		ListLink *l = listget(k);
		if (l == 0)
			return 0;

		Object *res = l->key;

		if (l->prev == 0)
			head = l->next;
		else
			l->prev->next = l->next;

		if (l->next == 0)
			tail = l->prev;
		else
			l->next->prev = l->prev;

		delete l;

		return res;
	}

	inline bool contains(Object *k)
	{
		ListLink *l = head;
		for ( ; l != 0; l = l->next)
			if (l->key == k)
				return true;
		return false;
	}

	inline void init()
	{
		current = head;
	}

	inline void initAtTail()
	{
		current = tail;
	}


	inline Object* getFirst()
	{
		return head == 0 ? 0 : head->key ;
	}

	inline Object* getLast()
	{
		return tail == 0 ? 0 : tail->key ;
	}

	inline bool hasNext()
	{
		return current != 0;
	}

	inline bool hasPrev()
	{
		return current != 0;
	}

	inline Object* next()
	{
		if (current == 0)
			return 0;

		Object *res = current->key;
		current = current->next;
		return res;
	}

	inline Object* prev()
	{
		if (current == 0)
			return 0;

		Object *res = current->key;
		current = current->prev;
		return res;
	}

	inline bool isEmpty() const
	{
		return head == 0;
	}

	inline int size() const
	{
		int count = 0;
		for (ListLink *l = head; l != 0; l = l->next)
			count++;
		return count;
	}

	inline Object* pop()
	{
		if (head == 0)
			return 0;

		ListLink *l = head;

		head = head->next;

		if (head != 0)
			head->prev = 0;
		else
			tail = 0;

		Object *res = l->key;
		delete l;

		return res;
	}

	std::string className() const override;
	void write(std::ostream &os) const override;
};
} // namespace util
#endif // ALUN_UTIL_LIST_H
