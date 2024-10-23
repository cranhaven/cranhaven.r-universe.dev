// List.h
//

#ifndef LIST_H
#define LIST_H

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <iostream>

using namespace std;

#define FAIL -1

template<class Item> 
struct Node
{
	Node* prev;
	Node* next;
	Item item;
};

template<class Item> class List
{
private:
	int size;
	Node<Item>* head;
	Node<Item>* tail;
public:
	List();
	List(const List<Item>& source);
	~List();
	void operator=(const List<Item>& source);
	void add(Item item);
	Item remove( int index);
	void insert(Item item, int index);
	void set(Item item, int index);
	int contains(Item item);
	void concat(List<Item> list);
	Item get( int index);
	void clear();
	void print();
	int getSize();
	bool isEmpty();
	Item* toArray();
};


template<class Item>
List<Item>::List()
{
	this->size = 0;
	this->head = NULL;
	this->tail = NULL;
}

template<class Item>
List<Item>::List(const List<Item>& source)
{
	this->size = 0;
	this->head = NULL;
	this->tail = NULL;

	if(source.size == 0)
	{
		return;
	}

	Node<Item>* temp = new Node<Item>;
	temp->item = source.head->item;
	temp->prev = NULL;
	head = temp;

	Node<Item>* current = source.head;

	for(int i = 0; i < source.size-1; i++)
	{
		current = current->next;
		temp->next = new Node<Item>;
		temp->next->item = current->item;
		temp->next->prev = temp;	
		temp = temp->next;
	}

	temp->next = NULL;
	tail = temp;

	size = source.size;
}

template<class Item>
List<Item>::~List()
{
	clear();
}

template<class Item>
void List<Item>::operator =(const List<Item>& source)
{
	Node<Item>* pDelete = head;

	while(head != NULL)
	{
		head = head->next;
		delete pDelete;
		pDelete = head;
	}

	size = 0;
	head = NULL;
	tail = NULL;

	if(source.size == 0)
	{
		return;
	}

	Node<Item>* temp = new Node<Item>;
	temp->item = source.head->item;
	temp->prev = NULL;
	head = temp;

	Node<Item>* current = source.head;

	for(int i = 0; i < source.size-1; i++)
	{
		current = current->next;
		temp->next = new Node<Item>;
		temp->next->item = current->item;
		temp->next->prev = temp;	
		temp = temp->next;
	}

	temp->next = NULL;
	tail = temp;

	size = source.size;
}

template<class Item>
bool List<Item>::isEmpty()
{
	if(size == 0)
	{
		return true;
	}
	else
	{
		return false;
	}
}

template<class Item>
void List<Item>::add(Item item)
{
	Node<Item>* temp = new Node<Item>;
	temp->item = item;
	
	if(this->head == NULL)
	{
		head = temp;
		head->prev = NULL;
		head->next = NULL;
		tail = head;
	}
	else
	{
		tail->next = temp;
		temp->prev = tail;
		temp->next = NULL;
		tail = temp;
	}

	size++;
}

template<class Item>
void List<Item>::set(Item item, int index)
{
	assert((index >= 0) && (index < size));

	Node<Item>* current = head;

	for(int i = 0; i < index; i++)
	{
		current = current->next;
	}

	current->item = item;
}

template<class Item>
Item List<Item>::get(int index)
{
	assert((index >= 0) && (index < size));

	Node<Item>* current = head;

	for(int i = 0; i < index; i++)
	{
		current = current->next;
	}
	
	return current->item;
}

template<class Item>
Item List<Item>::remove(int index)
{
	assert((index >= 0) && (index < size));

	Node<Item>* current = head;

	for(int i = 0; i < index; i++)
	{
		current = current->next;
	}

	if(current->prev != NULL)
	{
		current->prev->next = current->next;	

		if(current->next != NULL)
		{
			current->next->prev = current->prev;
		}
	}

	if(size == 1)
	{
		head = NULL;
		tail = NULL;
	}

	if(index == 0)
	{
		head = current->next;
	}

	if(index == size-1)
	{
		tail = current->prev;
	}

	size--;

	Item item = current->item;

	delete current;

	return item;
}

template<class Item>
void List<Item>::concat(List<Item> list)
{
	for(int i = 0; i < list.size; i++)
	{
		add(list.get(i));
	}
}

template<class Item>
int List<Item>::getSize()
{
	return size;
}

template<class Item>
void List<Item>::insert(Item item, int index)
{
	assert((index >= 0) && (index < size) || (size == 0) && (index == 0));
	
	Node<Item>* node = new Node<Item>;
	node->item = item;
	
	Node<Item>* current = head;
	
	for(int i = 0; i < index; i++)
	{
		current = current->next;
	}
	
	if(size != 0)
	{
		node->prev = current->prev;
		node->next = current;
		current->prev = node;
		
		if(node->prev != NULL)
		{
			node->prev->next = node;
		}
	}
	else
	{
		head = node;
		tail = node;
		node->next = NULL;
		node->prev = NULL;
	}
	
	if(index == 0)
	{
		head = node;
	}
	
	size++;
}

template<class Item>
void List<Item>::print()
{
	Node<Item>* current = head;

	for(int i = 0; i < size; i++)
	{
		std::cout << current->item << " ";
		current = current->next;
	}

	std::cout << endl;
}

template<class Item>
void List<Item>::clear()
{
	while(head != NULL)
	{
		Node<Item>* pDelete = head;
		head = head->next;
		delete pDelete;
	}

	this->size = 0;
	this->head = NULL;
	this->tail = NULL;
}

template<class Item>
Item* List<Item>::toArray()
{
	Item* array = new Item[size];

	Node<Item>* curr = head;

	int i = 0;

	while(curr!= NULL)
	{
		array[i] = curr->item;
		curr = curr->next;
		i++;
	}

	return array;
}

template<class Item>
int List<Item>::contains(Item item)
{
	Node<Item>* current = head;

	for(int i = 0; i < size; i++)
	{
		if(current->item == item)
		{
			return i;
		}

		current = current->next;
	}

	return FAIL;
}

#endif
