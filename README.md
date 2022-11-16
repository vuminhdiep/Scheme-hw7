Author: Emma & Vu Le

1.The method of garbage collection you are implementing.

We are using Mark-Sweep algorithm (Tracing Garbage Collection) which is done in 2 steps:

First, distinguish the reachable/live objects from the garbage (Mark phase): for each of the ref-val in the environment (env), we traverse the heap (the-store!) using depth first search to mark-bit them as reachable (true)
Then, reclaim the garbage (Sweep phase): clears the heap memory for all the free cells and add to a linkedlist of free cells. After all objects whose mark-bit are false are cleared from the-store!, what's left are all the reachable objects. Then the mark value for all reachable objects is set to false since we will run the algorithm again (if required)

It is a tracing garbage collector since it traces out the entire collection of objects that are directly and indirectly accessible by the env.

We are doing static mark-sweep so like it's called in the REP loop at start instead of dynamic which is doing garbage collecting as we creating new refs or other operations

2.The overall structure of your solution.

For the-store! (the heap), we use Scheme vector to implement and the-store! has a store-cell that contains an expval-cell (the cell with value as expval type) and a boolean mark-bit (true if the object is reachable from root, otherwise false - a.k.a free-cell). The free-cell also contains a pointer to the next one and all the free-cells are kept in a linkedlist-like structure

The environment (env) keep tracks of all local variables ref-val and we use each of the ref-val in env to mark in the-store! whether they are reachable or not by doing depth first search. After marking all the reachable objects, the garbage collector did the sweeping by assigning head-free! and tail-free! pointers to all the free-cells.

Also the-store! will double the size once it reaches the max capacity, and we also keep track of how many elements currently in the-store! to determine when to double the size and copy.

When doing depth first search in the-store!, we need to consider different cases whether the element in the-store! is a proc-val, ref-val or list-val to handle them.

The garbage collector is called in the REP loop statically, so whenever we want to invoke garbage collector we use command !force-gc

3.The function of individual functions as parts of the solution.

In store.scm:
(define-datatype store-cell store-cell?): create new data structure for the-store! to keep track of reachable objects and free-cells
(define copy-vector!): for copying the old store to another one
(define double-store!): double the size of the store when reaching max capacity by copying all elements to another store with double capacity
(define dfs-from): helper function to do depth first search on the-store! within each ref-val in the environment 
(define mark): calling dfs-from to search in the-store! and mark reachable objects or not
(define mark-for-list): calling dfs-from to mark elements in the list if a reachable object in the-store! is a list of ref-vals
(define sweep): putting all the free-cells that are not marked to a linkedlist with head-free! and tail-free!

In interp.scm:
!env raw: printing everything from the environment for debugging
!store: pringting everything from the-store! for debugging
!store-size: printing the size of thestore!
!reset-env: reset the environment to init-env
!reset-store: reset the-store! to empty on the init-env environment
!force-gc: invoke the garbage collector to run

Note: to keep it easy to debug, we decided to print out all the contents of the store! raw, including the (free-cell -1) that hasn't been allocated yet. The store will have an initial size of 2, so we can test the double-size mechanism more often. 


4.Trade-offs that you may have made in performance, robustness, or complexity.
The complexity of the mark phase is O(L), where L is the size of the reachable objects in the heap; the complexity of the sweep phase is O(H) where H is the size of the heap. In this case H > L.

Pros:
Mark-sweep algorithm is the basic, most common algorithm for garbage collector. 
There are no additional overheads incurred during the execution of algorithm.
The algorithm handles cyclic references well and never results in an infinite loop.
The Mark-Sweep algorithm doesn't create drag on every single memory operation like Reference Counting.

Cons:
However, there can be a cost of resetting the mark-bit of reachable objects after done sweeping. 
Every location in memory must be examined during the sweep stage of this algorithm - this can be time-consuming.
Can leave several gaps in used memory when objects are swept out. This is fragmentation has a negative impact on the overall throughput as it makes object allocation more difficult and garbage collections more frequent. 

5.Alternatives you tried before arriving at your current solution.
We tried to look at other algorithms like Reference counting, escape analysis to decide on which algorithm to choose. We read the Garbage Collector Handbook, by Anthony Hosking, Eliot Moss, Richard Jones besides the paper in the homework description to see which algorithm is the most possible to implement. The reason why we chose mark-sweep was because tracing garbage collector was supposedly the easiest, most common method to implement given a short timeframe with a lot of ambuigity. For Reference counting, you have to break the cycle by hand and collect the cyclic garbage, free space is never compacted and the reference counts need to be adjusted on most pointer write operation so it makes more sense to do depth first search in mark-sweep. And for escape analysis, there were fewer resources and ocumentations than mark-sweep. In addition, you would have to change the entire structure of the code since it requires interference in every newref!, deref and based on the surrouding context infer the life cycle of that object. This cannot be simply done in 2 weeks. 

6.If applicable, a description of which group members were responsible for which part of the work.
We did everything together

#Test 1: 
def! g = let y = 42 in let f = proc(x) y in f
(g 12)

#Test 2: 
def! r = let r1 = newref!(0) in let r2 = newref!(r1) in { setref!(r1,r2)  r1 }
deref(r)
deref(deref(r))

#Test 3:
def! p = newref!(0)
setref!(p,newref!(1))
setref!(deref(p),newref!(2))

#Test 4: Memory leak
def! x = newref!(0)
setref!(x,newref!(1))

#Test 5: self-reference
def! p = newref!(0)
setref!(p, deref(p))

#Test 6: cycle, two ref pointing to each other
def! p1 = newref!(0)
def! p2 = newref!(1)
setref!(p1, newref!(deref(p2)))
setref!(p2, newref!(deref(p1)))

#Test 8: unreachable cycle
def! p1 

#Test 9: Can allocate twice

#Test 10: root is not recycled

#Test 11: test_reachable_objects_not_collected

#Test 12:
def! x = cons(10, cons(12, emptylist))

#Test 13: Test with list
def! x = (emptylist)

#Test 14: test_root_cycle

#Test 15: test_nearly_full_heap

#Test 16: test_full_heap and every object is reachable

#Test 17:
def! p1 = newref!(0)
def! p2 = newref!(1)
def! x = cons(10, cons(deref(p1), deref(p2)))
