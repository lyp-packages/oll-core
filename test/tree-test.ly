\pinclude "../package.ly"

% create tree object
#(display "(oll:tree-create 'my-tree) : ")
#(newline)
mytree = #(oll:tree-create 'my-tree)

#(oll:tree-set! mytree '(x y z) #f) % set value #f at x/y/z
#(oll:tree-set! mytree '(a b c) #t) % set value #t at a/b/c
#(oll:tree-set! mytree '(a b) 42) % set value 42 at a/b
#(oll:tree-set! mytree '(global) 24) % set value 24 at global
#(display mytree) % display tree

#(display "-----------------------------------------")
#(newline)
#(display "(oll:tree-get mytree '(a)) : ")
#(display (oll:tree-get mytree '(a)))
#(newline)
#(display "(oll:tree-get mytree '(a b)) : ")
#(display (oll:tree-get mytree '(a b)))
#(newline)
#(display "(oll:tree-get mytree '(a b c)) : ")
#(display (oll:tree-get mytree '(a b c)))
#(newline)
#(display "(oll:tree-get mytree '(x y)) : ")
#(display (oll:tree-get mytree '(x y)))
#(newline)
#(display "(oll:tree-get-node mytree '(x y)) : ")
#(display (oll:tree-get-node mytree '(x y)))
#(newline)
#(display "(oll:tree-get mytree '(x y z)) : ")
#(display (oll:tree-get mytree '(x y z)))
#(newline)
#(display "(oll:tree-get-node mytree '(x y z)) : ")
#(display (oll:tree-get-node mytree '(x y z)))
#(newline)
#(display "-----------------------------------------")
#(newline)
%
#(display "(oll:tree-get-from-path mytree '(a b c d e f) 'b) : ")
#(display (oll:tree-get-from-path mytree '(a b c d e f) 'b))
#(newline)
#(display "(oll:tree-get-node-from-path mytree '(a b c d e f) 'b) : ")
#(display (oll:tree-get-node-from-path mytree '(a b c d e f) 'b))
#(newline)
#(display "(oll:tree-get-node-from-path mytree '(a b c d e f) 'not-found) : ")
#(display (oll:tree-get-node-from-path mytree '(a b c d e f) 'not-found))
#(newline)
#(display "(oll:tree-get-node-from-path mytree '(a b c d e f) 'x #f) : ")
#(display (oll:tree-get-node-from-path mytree '(a b c d e f) 'x #f))
#(newline)

% return pair with extra-path and value fond within path
#(display "(oll:tree-dispatch mytree '(a b c d e f)) : ")
#(display (oll:tree-dispatch mytree '(a b c d e f)))
#(newline)
% collect all values found on path
#(display "(oll:tree-collect mytree '(a b c d e f)) : ")
#(display (oll:tree-collect mytree '(a b c d e f)))
#(newline)

#(display "-----------------------------------------")
#(newline)
% TBD explain oll:tree-merge!
#(display "(oll:tree-merge! mytree '(a b) + 33) : ")
#(newline)
#(oll:tree-merge! mytree '(a b) + 33)
#(display mytree)

% a/b/d can only accept string? now
#(oll:tree-set-type! mytree '(a b d) string?)
% issues a warning and doesn't set the value
#(oll:tree-set! mytree '(a b d) 234)
#(oll:tree-set! #t mytree '(a b d) "234")
% This doesn't set the value as a/b/e/f doesn't exist
#(oll:tree-set! #f mytree '(a b e f) 123)
% This works because a is present
#(oll:tree-set! #f mytree '(a) "Oops")

% TBD explain oll:tree-merge!
#(oll:tree-set! mytree '(mods) #{ \with { \override NoteHead.color = #red } #})
#(oll:tree-merge! mytree '(mods) (lambda (m1 m2) #{ \with { $m1 $m2 } #}) #{ \with { \override Beam.color = #red } #})
#(display "(oll:tree-create 'my-other-tree)")
#(newline)
mytreeB = #(oll:tree-create 'my-other-tree)
#(oll:tree-set! mytreeB '(a b) 42) % set value 42 at a/b
#(oll:tree-set! mytreeB '(global) 24) % set value 24 at global
#(display mytreeB)
#(display "(oll:tree-merge! mytree + mytreeB) : ")
#(newline)
#(oll:tree-merge! mytree + mytreeB)

#(display mytree)
