#' @include utils.R

# Set
methods::setClass("CppSet",
  slots = c(
    pointer = "externalptr",
    type = "character"
  )
)

# Unordered set
methods::setClass("CppUnorderedSet",
  slots = c(
    pointer = "externalptr",
    type = "character"
  )
)

# Multiset
methods::setClass("CppMultiset",
  slots = c(
    pointer = "externalptr",
    type = "character"
  )
)

# Unordered multiset
methods::setClass("CppUnorderedMultiset",
  slots = c(
    pointer = "externalptr",
    type = "character"
  )
)

# Map
methods::setClass("CppMap",
  slots = c(
    pointer = "externalptr",
    key_type = "character",
    value_type = "character"
  )
)

# Unordered map
methods::setClass("CppUnorderedMap",
  slots = c(
    pointer = "externalptr",
    key_type = "character",
    value_type = "character"
  )
)

# Multimap
methods::setClass("CppMultimap",
  slots = c(
    pointer = "externalptr",
    key_type = "character",
    value_type = "character"
  )
)

# Unordered multimap
methods::setClass("CppUnorderedMultimap",
  slots = c(
    pointer = "externalptr",
    key_type = "character",
    value_type = "character"
  )
)

# Stack
methods::setClass("CppStack",
  slots = c(
    pointer = "externalptr",
    type = "character"
  )
)

# Queue
methods::setClass("CppQueue",
  slots = c(
    pointer = "externalptr",
    type = "character"
  )
)

# Priority queue
methods::setClass("CppPriorityQueue",
  slots = c(
    pointer = "externalptr",
    type = "character",
    ascending = "logical"
  )
)

# Vector
methods::setClass("CppVector",
  slots = c(
    pointer = "externalptr",
    type = "character"
  )
)

# Deque
methods::setClass("CppDeque",
  slots = c(
    pointer = "externalptr",
    type = "character"
  )
)

# Forward list
methods::setClass("CppForwardList",
  slots = c(
    pointer = "externalptr",
    type = "character"
  )
)

# List
methods::setClass("CppList",
  slots = c(
    pointer = "externalptr",
    type = "character"
  )
)
