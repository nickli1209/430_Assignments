#lang typed/racket
;;lecture notes monday week9 and wednesday week9, friday week9 (mem manage)

;;memory managment
;;in zode 6 we had the ability to add or alloc memory, but didint have the ability
;;didnt implement the ability to free

;;disadvantages of:

;;manual mem manage:
;;- more wokr to get right
;;- dont/forget to free mem (leaks)
;;- use after freeign or double free
;;- 

;;Automatic;
;;- typically slower
;;- typically more mem is used
;;- memory leaks can still happen... worse you have no control
;;however this is much more uncommon
;;-


;;what is a garbage collector (quiz tmr)
;;its job is to collect the garbage that that the programmer left behind

;;it is a runtime algorithm, (as opposed to the typechecker which is a static alg)
;;that proves that memory can be reclaimed (checkes to see that its no longer used)

;;SoundGarbageCollector
;;-if the GC can prove memory can be reclaimed, then the memory will never be used again
;;meaning its safe to reuse, (as soon as its never used again)

;;CompleteGC
;;-if memory is never used again, then the GC prove it can be reclaimed

;;interms of avoiding bugs
;;sound GC avoids the use after free bug

;;a complete GC will never have memory leaks, it will reclaim all

;;**Having a sound GC is muchmore important, using memory after its been freed is a much worse error
;;then leaking and wasting memory

;;TRIVIALLY SOUND GC
;;- never free anything

;;Trivially complete GC
;;- free everythng immediatly

;;FIRST GARBAGE COLLECTION STRATEGY O(allocated)
;;we have a heap, and a memory storage (store)
;;if the memory is unreachable from the heap, collect it, cant be used
;;if its is reachable, dont collect it, it could still be used
;;(unreachable meaning when a dfs is ran from heap, those spots arent visited)

;;(mark and sweep)
;;traverse reachable blocks and mark them using dfs O(reachable)
;;free all unreachable blocks O(all allocated)

;;SECOND GARBAGE COLLECTOR (copy collector) O(reachable)
;;Space Collector
;;- traverse reachable blocks, and move them to the other space
;;- then need to maintain forwarding address in case mem is moved, so later moves can know how
;;to update references
;;then the first (warehouse) is empty/free because they have been copied to a new warehouse
;;this is faster than mark and sweep as this is O(reachable) vs O(allocated)as seen in sweep
;;-the major downside of thsi method is you can only use half of the max memory you have,
;;because the second half needs to be reserved for copying and moving

;;where this causes poor performance is when memory is very close to half memory, it can get stuck
;;copying back and forth
;;it is good to use if max memory is never goign to be reached/ low memory applications


;;THIRD GARBAGE COLLECTION (copy collector)O(reachable)
;;divide into three spaces
;;- first space is where all new memory allocatiosn happen
;;- if this fills up than, the still reachable memory gets copied down into space 2
;;- then all new memory is still allocated in space 1, and if space 1 fils up again,
;;you copy the still reachable memory down into space2
;;- if generation 2/space 2 were ever to fill up, then you take all the still living/reachable memory
;;in generation 2, and copy it down to generation 3
;;- now generation 2 is empty, and many cycles will occur from 1 to 2 again
;;this method greatly reduces the risk of thrashing

;;what makes this difficult to impliment is cross generational referencing

;;FRIDAY:

;;Automatic memory management:
;;- Mark and sweep
;;- 2-space Collector
;;- Gnerational Collector

;;operated on the idea, stop the world, do a graph traversal, located the unreachable memory
;;thus only when the travesal is run, would all unreachable mem get collected

;;New Idea: try continuously keep track of what is reachable, then as sson as something becomes unreachable remove it

;;to do this we need to keep track of all memory, that refers to a given piece of memory
;;A count of how many references to itself exist
;;This strategy is called reference counting

;;in order to do this, reference counts must be managed and updated according to the flow of memory
;;to maintain accuracy

;;the idea is rather than having spikes of high time complexity where a grabage collection is running as in
;;the previous three strategies, in reference counting, the entire systems will run slightly slower to allow
;;for managing of ref counts, but then there will be no pasues to run a GC

;;steps:
;;- keep track of ref count for each bit of mem
;;when a mem hits zero, decriment the ref count of all neighbors
;;then free accordingly

;;cons:
;;can have gaps in memory
;;extra mem use for ref counts

;;big problem: cyclic data can lead to memory leaks
;;how to deal with cycles?
;;all other methods that do a depth first search would catch this

;;to modify ref counting to make this work...
;;- Tell programs not to make cyclic data...
;;- Add a tool/feature to language (weak references), maybe some references,
;;dont influence ref count, ie the part that makes the data a circle, can be a weak reference
;;that doesnt influence the ref count, allowing the free to occur
;;- if memory fills up or gets close, fal back to one of the three DFS based
;;(stop the world)approaches to ensure the unused memory gets free'd

;;when do these run"
;;-generation and 2-space, both run when a chunk of memory fills up
;;-ref counting runs anytime a ref count drops to zero


;;on other method we didnt go into deeply is Ownership and borrowing strategy, (rust uses this)
;;

