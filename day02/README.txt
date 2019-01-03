Inventory Management System

No surprise there.

For part one, reduce each line to a pair of booleans "is there a
two-occurence letter/is there a three-occurence letter".  Then
wastefully traverse it twice counting the Trues, sum and multiply.
Refactoring to traverse it just once is simply not worth it.

For part two, for each pair of lines compare the characters one by
one, and report those that match when the count that doesn't is one or
less.  With a 250-line input, this quadratic scheme is more than fast
enough.
