# Pattern Match Compilation

Emelle uses Luc Maranget's algorithm for pattern match compilation as described
in "Compiling Pattern Matching to Good Decision Trees."

    p ::= C p1 .. pN  -- Constructor pattern
          _           -- Wildcard pattern

    tree ::= Leaf of action
               -- Jump to code
             Switch of register * operand * (C -> register list * tree) * tree
               -- Switch on constructor of the given operand

Let there be a matrix of patterns. The algorithm to compile this matrix into a
decision tree is as follows:

- If there are no rows, then produce a fail node.
- If the top row is all wildcards, then produce a leaf node.
- Otherwise, there should exist at least one column with a constructor pattern.
  Choose such a column. In my implementation, I simply traverse the first row
  row, returning the index of the constructor if one exists.
- If the index isn't the first one, move the column of that index to the front
  of the matrix.
- For each constructor of the algebraic data type, do the matrix specialization
  operation, then compute the decision tree of the specialized matrix.
- Find the default matrix.
- The `Ref` pattern is like a one-argument constructor pattern that cannot fail.
- The `()` pattern is like a wildcard.

## Matrix specialization

Given a matrix and a constructor:

Remove the rows that begin with a constructor that doesn't match. For the rows
with a matching constructor, pop the front pattern off the row and push its
children on the row. For the rows that begin with a wildcard, pop off the
wildcard and push wildcards for each type in the constructor's product type.

This operation is akin to popping a successfully matched scrutinee off the
pattern match stack and pushing its subterms on the stack.

## Default matrix

Discard the rows that start with a constructor. For each row that starts with a
wildcard, pop off the wildcard.

This operation is akin to popping off a scrutinee that failed to match.

## Binding variables

The patterm match compiler maintains a stack of operands. For the child decision
tree from matrix specialization, pop the front of the stack and push operands
that hold the children of the popped operand. For the child decision tree from
the default matrix, pop the front of the stack and don't push anything.

Each constructor case not only maps to a subtree, but also a list of registers,
as these registers each hold a subterm of the matched operand.
