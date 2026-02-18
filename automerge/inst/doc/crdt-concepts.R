## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(automerge)

## -----------------------------------------------------------------------------
doc1 <- am_create()
doc1[["name"]] <- "Alice"
doc1[["score"]] <- 100
am_commit(doc1)

doc2 <- am_fork(doc1)

# Concurrent edits
doc1[["name"]] <- "Alice Smith"
doc2[["name"]] <- "Alice Johnson"

# Merge
am_merge(doc1, doc2)

# One value wins (deterministic, all replicas agree)
doc1[["name"]]

# To see all conflicting values (not just the winner), use am_map_get_all()
all_values <- am_map_get_all(doc1, AM_ROOT, "name")
length(all_values) # 2 - both "Alice Smith" and "Alice Johnson"

am_close(doc1)
am_close(doc2)

## -----------------------------------------------------------------------------
doc3 <- am_create()
doc3[["user"]] <- list(name = "Alice", age = 30L, city = "Boston")
am_commit(doc3)

doc4 <- am_fork(doc3)

# Concurrent edits to different keys
user3 <- am_get(doc3, AM_ROOT, "user")
am_put(doc3, user3, "age", 31L)

user4 <- am_get(doc4, AM_ROOT, "user")
am_put(doc4, user4, "city", "New York")

# Merge - both changes preserved
am_merge(doc3, doc4)

# Both edits are present
user_final <- am_get(doc3, AM_ROOT, "user")
am_get(doc3, user_final, "age")
am_get(doc3, user_final, "city")

am_close(doc3)
am_close(doc4)

## -----------------------------------------------------------------------------
doc5 <- am_create()
am_put(doc5, AM_ROOT, "items", AM_OBJ_TYPE_LIST)
items5 <- am_get(doc5, AM_ROOT, "items")

am_put(doc5, items5, "end", "A")
am_put(doc5, items5, "end", "C")
am_commit(doc5)

doc6 <- am_fork(doc5)

# Concurrent insertions at same position
items6 <- am_get(doc6, AM_ROOT, "items")
am_insert(doc5, items5, 2, "B1") # Insert between A and C
am_insert(doc6, items6, 2, "B2") # Insert between A and C

# Merge
am_merge(doc5, doc6)

# Both insertions preserved with deterministic ordering
for (i in seq_len(am_length(doc5, items5))) {
  print(am_get(doc5, items5, i))
}

am_close(doc5)
am_close(doc6)

## -----------------------------------------------------------------------------
# Map conflict: two peers edit the same key concurrently
doc_c1 <- am_create()
doc_c1[["status"]] <- "draft"
am_commit(doc_c1)

doc_c2 <- am_fork(doc_c1)

doc_c1[["status"]] <- "published"
am_commit(doc_c1)
doc_c2[["status"]] <- "archived"
am_commit(doc_c2)

am_merge(doc_c1, doc_c2)

# am_get returns the winner
am_get(doc_c1, AM_ROOT, "status")

# am_map_get_all returns all conflicting values
all_statuses <- am_map_get_all(doc_c1, AM_ROOT, "status")
length(all_statuses) # 2
all_statuses

## -----------------------------------------------------------------------------
# List conflict: two peers update the same index
doc_l1 <- am_create()
am_put(doc_l1, AM_ROOT, "scores", AM_OBJ_TYPE_LIST)
scores <- am_get(doc_l1, AM_ROOT, "scores")
am_insert(doc_l1, scores, 1, 100L)
am_commit(doc_l1)

doc_l2 <- am_fork(doc_l1)
scores2 <- am_get(doc_l2, AM_ROOT, "scores")

am_put(doc_l1, scores, 1, 200L)
am_commit(doc_l1)
am_put(doc_l2, scores2, 1, 300L)
am_commit(doc_l2)

am_merge(doc_l1, doc_l2)

# Winner
am_get(doc_l1, scores, 1)

# All conflicting values
am_list_get_all(doc_l1, scores, 1)

am_close(doc_c1)
am_close(doc_c2)
am_close(doc_l1)
am_close(doc_l2)

## -----------------------------------------------------------------------------
doc7 <- am_create()
am_put(doc7, AM_ROOT, "document", am_text("The quick fox jumps"))
am_commit(doc7)

doc8 <- am_fork(doc7)

text7 <- am_get(doc7, AM_ROOT, "document")
text8 <- am_get(doc8, AM_ROOT, "document")

# Concurrent edits (0-based inter-character positions)
am_text_splice(text7, 10, 0, "brown ") # Insert "brown " at position 10
am_text_splice(text8, 19, 0, " high") # Insert " high" at position 19 (end)

# Merge
am_merge(doc7, doc8)

# Both edits preserved
am_text_content(text7)

am_close(doc7)
am_close(doc8)

## -----------------------------------------------------------------------------
# String (deterministic conflict resolution)
doc9 <- am_create()
doc9[["title"]] <- "Document"
doc10 <- am_fork(doc9)

doc9[["title"]] <- "My Document"
doc10[["title"]] <- "Our Document"
am_merge(doc9, doc10)
doc9[["title"]] # One value wins deterministically

am_close(doc9)
am_close(doc10)

# Text object (CRDT)
doc11 <- am_create()
am_put(doc11, AM_ROOT, "content", am_text("Hello"))
doc12 <- am_fork(doc11)

text11 <- am_get(doc11, AM_ROOT, "content")
text12 <- am_get(doc12, AM_ROOT, "content")

am_text_splice(text11, 5, 0, " World")
am_text_splice(text12, 5, 0, " Everyone")
am_merge(doc11, doc12)

am_text_content(text11)

am_close(doc11)
am_close(doc12)

## -----------------------------------------------------------------------------
doc13 <- am_create()
am_put(doc13, AM_ROOT, "likes", am_counter(0))
am_commit(doc13)

doc14 <- am_fork(doc13)

# Concurrent increments
am_counter_increment(doc13, AM_ROOT, "likes", 3)
am_counter_increment(doc14, AM_ROOT, "likes", 5)
am_counter_increment(doc14, AM_ROOT, "likes", -1)

# Merge
am_merge(doc13, doc14)

# Sum of all increments
doc13[["likes"]]

am_close(doc13)
am_close(doc14)

## -----------------------------------------------------------------------------
doc15 <- am_create()
doc15[["created_at"]] <- Sys.time()
am_commit(doc15)

Sys.sleep(0.1)

doc16 <- am_fork(doc15)

doc15[["updated_at"]] <- Sys.time()
doc16[["updated_at"]] <- Sys.time()

am_merge(doc15, doc16)

doc15[["created_at"]]
doc15[["updated_at"]]

am_close(doc15)
am_close(doc16)

## -----------------------------------------------------------------------------
doc17 <- am_create()
am_put(doc17, AM_ROOT, "text", am_text("Hello World"))
text17 <- am_get(doc17, AM_ROOT, "text")

# Create cursor at position 6 (0-based: after "Hello ")
cursor <- am_cursor(text17, 6)

# Insert text before cursor
am_text_splice(text17, 0, 0, "Hi ")

# Cursor automatically adjusts
new_pos <- am_cursor_position(cursor)
new_pos # Cursor moved with text from original position 6

## -----------------------------------------------------------------------------
# Serialize to bytes or string
cursor_bytes <- am_cursor_to_bytes(cursor)
cursor_str <- am_cursor_to_string(cursor)

# Restore later (requires the text object)
restored <- am_cursor_from_bytes(cursor_bytes, text17)
am_cursor_position(restored)

am_cursor_equal(cursor, restored) # TRUE

am_close(doc17)

## -----------------------------------------------------------------------------
doc18 <- am_create()
am_put(doc18, AM_ROOT, "text", am_text("Hello World"))
text18 <- am_get(doc18, AM_ROOT, "text")

# Mark "Hello" as bold (positions 0-4, 0-based)
am_mark(text18, 0, 5, "bold", TRUE, expand = "none")

# Mark "World" as italic (positions 6-10)
am_mark(text18, 6, 11, "italic", TRUE, expand = "none")

# Query marks
marks <- am_marks(text18)
str(marks)

# Marks at specific position
marks_at_pos <- am_marks_at(text18, 2) # Position 2 (in "Hello")
str(marks_at_pos)

am_close(doc18)

## -----------------------------------------------------------------------------
doc18b <- am_create()
am_put(doc18b, AM_ROOT, "text", am_text("Hello World"))
text18b <- am_get(doc18b, AM_ROOT, "text")

am_mark(text18b, 0, 11, "bold", TRUE)
length(am_marks(text18b)) # 1

am_mark_clear(text18b, 0, 11, "bold")
length(am_marks(text18b)) # 0

am_close(doc18b)

## -----------------------------------------------------------------------------
doc19 <- am_create()
am_put(doc19, AM_ROOT, "text", am_text("Hello"))
text19 <- am_get(doc19, AM_ROOT, "text")

# Mark with expansion
am_mark(text19, 0, 5, "bold", TRUE, expand = "after")

# Insert at end of mark
am_text_splice(text19, 5, 0, " World")

# Mark expands to include "World"
marks <- am_marks(text19)
str(marks)

am_close(doc19)

## -----------------------------------------------------------------------------
doc20 <- am_create()

# Make many edits
for (i in 1:100) {
  doc20[[paste0("key", i)]] <- i
}
am_commit(doc20)

# Size includes all history
length(am_save(doc20))

am_close(doc20)

## -----------------------------------------------------------------------------
# Map: Deletion vs concurrent update - update wins
doc21 <- am_create()
doc21[["temp"]] <- "value"
am_commit(doc21)

doc22 <- am_fork(doc21)

am_delete(doc21, AM_ROOT, "temp")
am_commit(doc21)

doc22[["temp"]] <- "updated"
am_commit(doc22)

am_merge(doc21, doc22)
doc21[["temp"]] # Update takes precedence over delete

am_close(doc21)
am_close(doc22)

## -----------------------------------------------------------------------------
# List: Delete and insert at same position - both operations apply
doc23 <- am_create()
am_put(doc23, AM_ROOT, "items", AM_OBJ_TYPE_LIST)
items23 <- am_get(doc23, AM_ROOT, "items")
am_put(doc23, items23, "end", "A")
am_put(doc23, items23, "end", "B")
am_put(doc23, items23, "end", "C")
am_commit(doc23)

doc24 <- am_fork(doc23)
items24 <- am_get(doc24, AM_ROOT, "items")

am_delete(doc23, items23, 2)
am_insert(doc24, items24, 2, "X")

am_merge(doc23, doc24)

for (i in seq_len(am_length(doc23, items23))) {
  print(am_get(doc23, items23, i))
}

am_close(doc23)
am_close(doc24)

## -----------------------------------------------------------------------------
# Good: Independent counters per user
doc_good <- am_create()
doc_good[["votes"]] <- list(
  alice = am_counter(0),
  bob = am_counter(0)
)

# Better than: Single counter for all votes
doc_bad <- am_create()
doc_bad[["total_votes"]] <- am_counter(0) # Loses attribution

am_close(doc_good)
am_close(doc_bad)

## -----------------------------------------------------------------------------
doc25 <- am_create()

# Good: Atomic transaction
doc25[["user"]] <- list(name = "Alice", age = 30L, city = "Boston")
am_commit(doc25, "Add user Alice")

# Bad: Many micro-commits (increases storage)
doc25[["status"]] <- "active"
am_commit(doc25, "Set status")
doc25[["role"]] <- "admin"
am_commit(doc25, "Set role")

am_close(doc25)

## -----------------------------------------------------------------------------
doc26 <- am_create()
doc26[["status"]] <- "draft"
am_commit(doc26)

doc27 <- am_fork(doc26)

doc26[["status"]] <- "published"
doc27[["status"]] <- "archived"

am_merge(doc26, doc27)

# One will win - application should handle both states sensibly
doc26[["status"]] # Should be prepared for either 'published' or 'archived'

# Use am_equal() to check if two documents have converged
am_merge(doc27, doc26)
am_equal(doc26, doc27) # TRUE - both have the same state now

am_close(doc26)
am_close(doc27)

