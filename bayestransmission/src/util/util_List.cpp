#include "util/List.h"
#include <ostream>

namespace util {

// ListLink

ListLink::ListLink(Object *k)
{
    key = k;
    prev = 0;
    next = 0;
}

std::string ListLink::className() const
{
    return "ListLink";
}

void ListLink::write(std::ostream &os) const
{
    Object::write(os);
    os << "(" << key << ")";
}

//List

List::List() : Object()
{
    head = 0;
    tail = 0;
    current = 0;
}

List::~List()
{
    clear();
}

Object* List::random(Random *r) const
{
    int x = size();
    if (x == 0)
        return 0;

    double u = r->runif() * x;
    x = 0;

    for (ListLink *l = head; l != 0; l = l->next)
    {
        if (u <= ++x)
            return l->key;
    }

    return 0;
}

void List::append(Object *k)
{
    ListLink *l = new ListLink(k);

    if (head == 0)
        head = l;

    if (tail != 0)
    {
        l->prev = tail;
        tail->next = l;
    }
    tail = l;
}

void List::prepend(Object *k)
{
    ListLink *l = new ListLink(k);

    if (tail == 0)
        tail = l;

    if (head != 0)
    {
        l->next = head;
        head->prev = l;
    }
    head = l;
}

std::string List::className() const
{
    return "List";
}

void List::write(std::ostream &os) const
{
    Object::write(os);
    for (ListLink *l = head; l != 0; l = l->next)
        os << "\n\t" << l;
}


} // namespace util
