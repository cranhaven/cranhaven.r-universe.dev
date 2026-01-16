// util/Map.h
#ifndef ALUN_UTIL_MAP_H
#define ALUN_UTIL_MAP_H
#include "Object.h"
#include "Random.h"

namespace util{

class MapLink : public Object
{
friend class Map;

private:
	Object *key;
	Object *value;

	MapLink *next;
	MapLink *prev;
	MapLink *tnext;
	MapLink *tprev;

	MapLink(Object *k)
	{
		key = k;
		value = 0;
		prev = 0;
		next = 0;
		tnext = 0;
		tprev = 0;
	}

	std::string className() const override
	{
		return "MapLink";
	}

	void write(std::ostream &os) const override
	{
		Object::write(os);
		os << "(" << key << "->" << value << ")";
	}
};

class Map : public Object
{
private:
	static const unsigned long defcap = 10UL;

	int cap;
	int use;
	double load;
	int maxocc;

	MapLink **tab;
	MapLink *head;
	MapLink *tail;
	MapLink *current;

	void ensure()
	{
		if (use+1 < cap * load)
			return;

		delete [] tab;
		cap *= 2;
		tab = new MapLink*[cap];
		for (int i=0; i<cap; i++)
			tab[i] = 0;

		for (MapLink *l = head; l != 0; l = l->next)
		{
			l->tnext = 0;
			l->tprev = 0;
		}

		for (MapLink *l = head; l != 0; l = l->next)
			tabadd(l);
	}

	inline int occupancy(MapLink *x) const
	{
		int occ = 0;
		for (MapLink *l = x; l != 0; l = l->tnext)
			occ++;
		return occ;
	}

	inline void tabadd(MapLink *l)
	{
		int i = where(l->key);
		if (tab[i] != 0)
			tab[i]->tprev = l;
		l->tnext = tab[i];
		tab[i] = l;

		int occ = occupancy(tab[i]);
		if (occ > maxocc)
			maxocc = occ;
	}

	inline void tabrem(MapLink *l)
	{
		int i = where(l->key);
		if (l->tprev == 0)
			tab[i] = l->tnext;
		else
			l->tprev->tnext = l->tnext;

		if (l->tnext != 0)
			l->tnext->tprev = l->tprev;

		int occ = occupancy(tab[i]);
		if (occ+1 == maxocc)
		{
			maxocc = 0;
			for (int j = 0; j < cap; j++)
			{
				occ = occupancy(tab[j]);
				if (occ > maxocc)
					maxocc = occ;
			}
		}
	}

	inline MapLink *tabrand(Random *r) const
	{
		if (use == 0)
			return 0;

		int i = 0;
		int j = 0;
		double u = 0;

		do
		{
			i = (int) (r->runif() * cap);
			j = occupancy(tab[i]);
			u = r->runif() * maxocc;
		}
		while (u > j);

		j = 0;
		for (MapLink *l = tab[i]; l != 0; l = l->tnext)
			if (u <= ++j)
				return l;

		return 0;
	}

	inline MapLink *tabgot(Object *o) const
	{
		for (MapLink *l = tab[where(o)]; l != 0; l = l->tnext)
			if (l->key == o)
				return l;
		return 0;
	}

	inline void listadd(MapLink *l)
	{
		l->prev = tail;
		if (tail != 0)
			tail->next = l;
		tail = l;

		if (head == 0)
			head = l;
	}

	inline void listrem(MapLink *l)
	{
		if (l->prev == 0)
			head = l->next;
		else
			l->prev->next = l->next;

		if (l->next == 0)
			tail = l->prev;
		else
			l->next->prev = l->prev;
	}

	inline int where(Object *o) const
	{
		if (o == 0)
			return 0;

		long res = o->hash() % cap;
		while (res < 0)
			res += cap;

		return res;
	}

public:

	Map(int c = defcap, double l = 0.75) : Object()
	{
		if (c < 1)
			c = 1;
		cap = c;
		load = l;

		head = 0;
		tail = 0;

		use = 0;

		tab = new MapLink*[cap];
		for (int i=0; i<cap; i++)
			tab[i] = 0;

		maxocc = 0;
	}

	~Map()
	{
		delete [] tab;

		for (MapLink *l = head; l != 0; )
		{
			MapLink *ll = l;
			l = l->next;
			delete ll;
		}
	}

	inline void clear()
	{
		for (MapLink *l = head; l != 0; )
		{
			MapLink *ll = l;
			l = l->next;
			delete ll;
		}

		for (int i=0; i<cap; i++)
			tab[i] = 0;

		head = 0;
		tail = 0;
		use = 0;
		maxocc = 0;
	}

	inline Map *copy()
	{
		Map *x = new Map(size(),0.75);
		for (init(); hasNext(); )
		{
			Object *k = next();
			x->put(k,get(k));
		}

		return x;
	}

	inline void put(Object *k, Object *v)
	{
		MapLink *l = tabgot(k);

		if (l == 0)
		{
			ensure();
			l = new MapLink(k);
			use++;
			tabadd(l);
			listadd(l);
		}

		l->value = v;
	}

	inline Object *get(Object *k) const
	{
		MapLink *l = tabgot(k);
		return l == 0 ? 0 : l->value;
	}

	inline void add(Object *k)
	{
		put(k,0);
	}

	inline bool got(Object *k) const
	{
		return tabgot(k) != 0;
	}

	inline Object *remove(Object *k)
	{
		MapLink *l = tabgot(k);
		if (l == 0)
			return 0;

		Object *res = l->key;

		listrem(l);
		tabrem(l);
		delete l;
		use--;

		return res;
	}

	inline void change(int opt, Object *k)
	{
		if (opt > 0)
			add(k);
		else if (opt < 0)
			remove(k);
	}

	inline int size() const
	{
		return use;
	}

	inline void init()
	{
		current = head;
	}

	inline bool hasNext() const
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

	inline Object* nextValue()
	{
		if (current == 0)
			return 0;
		Object *res = current->value;
		current = current->next;
		return res;
	}

	inline Object *getFirstKey() const
	{
		return head == 0 ? 0 : head->key;
	}

	inline Object *getFirstValue() const
	{
		return head == 0 ? 0 : head->value;
	}

	inline Object *getLastKey() const
	{
		return tail == 0 ? 0 : tail->key;
	}

	inline Object *getLastValue() const
	{
		return tail == 0 ? 0 : tail->value;
	}

	inline Object *randomKey(Random *r) const
	{
		return tabrand(r)->key;
	}

	inline Object *randomValue(Random *r) const
	{
		return tabrand(r)->value;
	}

	std::string className() const override
	{
		return "Map";
	}

	void write (std::ostream &os) const override
	{
		Object::write(os);
		os << "(" << use << "/" << cap << ")";
		for (MapLink *l = head; l != 0; l = l->next)
			os << "\n\t" << l;
	}
};
} // namespace util
#endif // ALUN_UTIL_MAP_H
