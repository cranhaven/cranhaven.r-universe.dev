## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
require("cppcontainers")

## -----------------------------------------------------------------------------
## set
s <- cpp_set(c(4, 2, 3))
s
insert(s, c(4, 6))
s
print(s, n = -2)

## unordered set
s <- cpp_unordered_set(3:5)
s
emplace(s, 2)
s
type(s)

## multiset
s <- cpp_multiset(c(3, 3, 6, 2))
s
contains(s, 2)
to_r(s)

## unordered multiset
s <- cpp_unordered_multiset(c(3, 3, 6, 2))
s
count(s, 3)
rehash(s)

## map
m <- cpp_map(c("Alice", "Bob"), c(TRUE, FALSE))
m
m["Bob"]
insert_or_assign(m, FALSE, "Alice")
m

## unordered map
m1 <- cpp_unordered_map(c("Alice", "Bob"), 3:4)
m1
m2 <- cpp_unordered_map(c("Bob", "Jane"), 6:7)
m2
merge(m1, m2)
m1
m2
at(m1, "Jane")

## multimap
m <- cpp_multimap(c("Alice", "Bob", "Bob"), 3:5)
m
clear(m)
empty(m)

## unordered multimap
m <- cpp_multimap(c("Alice", "Bob", "Bob"), 3:5)
m
size(m)
erase(m, "Bob")
m

## stack
s1 <- cpp_stack(3:4)
s1
s2 <- cpp_stack(3:4)
s2
s1 == s2
pop(s1)
s1

## queue
q <- cpp_queue(c(2.1, 3.3))
q
push(q, 2.7)
q
back(q)

## priority queue
p <- cpp_priority_queue(c(2.4, 1.3, 4.2, 1.5))
p
top(p)
sorting(p)

## vector
v <- cpp_vector(2:4)
v
reserve(v, 10)
capacity(v)

## deque
d <- cpp_deque(c("Alice", "Bob", "Bob"))
d
push_front(d, "Jane")
d
pop_front(d)
d

## forward list
l <- cpp_forward_list(c(4, 2, 6))
l
sort(l)
l
resize(l, 7)
l

## list
l <- cpp_list(c("Alice", "Bob", "Bob", "Alice"))
unique(l)
l
print(l, from = 3)

