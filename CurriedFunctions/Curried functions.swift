infix operator |>  {
    associativity left
    precedence 95
}

func |><I, O>(input: I, transform: I -> O) -> O {
    return transform(input)
}


/// Return the result of advancing start by `n` positions, or until it
/// equals `end`.  If `T` models `RandomAccessIndexType`, executes in
/// O(1).  Otherwise, executes in O(`abs(n)`).  If `T` does not model
/// `BidirectionalIndexType`, requires that `n` is non-negative.
func advance<T : ForwardIndexType>(n: T.Distance, end: T)(start: T) -> T {
    return advance(start, n, end)
}


/// Return the result of advancing `start` by `n` positions.  If `T`
/// models `RandomAccessIndexType`, executes in O(1).  Otherwise,
/// executes in O(`abs(n)`).  If `T` does not model
/// `BidirectionalIndexType`, requires that `n` is non-negative.
///
/// `advance(i, n)` is a synonym for `i++n'
func advance<T: ForwardIndexType>(n: T.Distance)(start: T) -> T {
    return advance(start, n)
}


/// Return `true` iff an element in `seq` satisfies `predicate`.
func contains<S : SequenceType, L : BooleanType>(@noescape predicate: (S.Generator.Element) -> L)(seq: S) -> Bool {
    return contains(seq, predicate)
}


/// Return `true` iff `x` is in `seq`.
func contains<S : SequenceType where S.Generator.Element : Equatable>(x: S.Generator.Element)(seq: S) -> Bool {
    return contains(seq, x)
}


/// Measure the distance between `start` and `end`.
///
/// If `T` models `RandomAccessIndexType`, requires that `start` and `end` are
/// part of the same sequence, and executes in O(1).
///
/// Otherwise, requires that `end` is reachable from `start` by
/// incrementation, and executes in O(N), where N is the function's
/// result.
func distance<T : ForwardIndexType>(end: T)(start: T) -> T.Distance {
    return distance(start, end)
}


/// Return true iff `a1` and `a2` contain equivalent elements, using
/// `isEquivalent` as the equivalence test.  Requires: `isEquivalent`
/// is an `equivalence relation
/// <http://en.wikipedia.org/wiki/Equivalence_relation>`_
func equal<S1 : SequenceType, S2 : SequenceType where S1.Generator.Element == S2.Generator.Element>(a2: S2, @noescape isEquivalent: (S1.Generator.Element, S2.Generator.Element) -> Bool)(a1: S1) -> Bool {
    return equal(a1, a2, isEquivalent)
}


/// Return `true` iff `a1` and `a2` contain the same elements in the
/// same order.
func equal<S1 : SequenceType, S2 : SequenceType where S1.Generator.Element == S2.Generator.Element, S1.Generator.Element : Equatable>(a2: S2)(a1: S1) -> Bool {
    return equal(a1, a2)
}


/// Append elements from `newElements` to `x`.  Complexity:
/// O(N)
func extend<C : RangeReplaceableCollectionType, S : CollectionType where C.Generator.Element == S.Generator.Element>(newElements: S)(inout x: C) {
    extend(&x, newElements)
}


/// Return an `Array` containing the elements of `source`,
/// in order, that satisfy the predicate `includeElement`.
func filter<S : SequenceType>(includeElement: (S.Generator.Element) -> Bool)(source: S) -> [S.Generator.Element] {
    return filter(source, includeElement)
}


/// Returns the first index where `value` appears in `domain` or `nil` if
/// `value` is not found.
///
/// Complexity: O(\ `countElements(domain)`\ )
func find<C : CollectionType where C.Generator.Element : Equatable>(value: C.Generator.Element)(domain: C) -> C.Index? {
    return find(domain, value)
}


/// Return an `Array` containing the results of mapping `transform`
/// over `source` and flattening the result.
func flatMap<S : SequenceType, T>(@noescape transform: (S.Generator.Element) -> [T])(source: S) -> [T] {
    return flatMap(source, transform)
}


/// Returns `f(self)!` iff `self` and `f(self)` are not nil.
func flatMap<T, U>(@noescape f: (T) -> U?)(x: T?) -> U? {
    return flatMap(x, f)
}


/// Insert `newElement` into `x` at index `i`.
///
/// Invalidates all indices with respect to `x`.
///
/// Complexity: O(\ `countElements(x)`\ ).
func insert<C : RangeReplaceableCollectionType>(newElement: C.Generator.Element, atIndex i: C.Index)(inout x: C) {
    insert(&x, newElement, atIndex: i)
}


/// Creates and returns a collection of type `C` that is the result of
/// interposing a given separator between the elements of the sequence
/// `elements`.
///
/// For example, this code excerpt writes "``here be dragons``" to the standard
/// output::
///
///   println(join(" ", [ "here", "be", "dragons" ]))
func join<C : ExtensibleCollectionType, S : SequenceType where C == S.Generator.Element>(elements: S)(separator: C) -> C {
    return join(separator, elements)
}


/// Return true iff a1 precedes a2 in a lexicographical ("dictionary")
/// ordering, using "<" as the comparison between elements.
func lexicographicalCompare<S1 : SequenceType, S2 : SequenceType where S1.Generator.Element == S2.Generator.Element, S1.Generator.Element : Comparable>(a2: S2)(a1: S1) -> Bool {
    return lexicographicalCompare(a1, a2)
}


/// Return true iff `a1` precedes `a2` in a lexicographical ("dictionary")
/// ordering, using `isOrderedBefore` as the comparison between elements.
///
/// Requires: isOrderedBefore` is a `strict weak ordering
/// <http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings>`__
/// over the elements of `a1` and `a2`.
func lexicographicalCompare<S1 : SequenceType, S2 : SequenceType where S1.Generator.Element == S2.Generator.Element>(a2: S2, @noescape isOrderedBefore less: (S1.Generator.Element, S1.Generator.Element) -> Bool)(a1: S1) -> Bool {
    return lexicographicalCompare(a1, a2, isOrderedBefore: less)
}


/// Haskell's fmap for Optionals.
func map<T, U>(f: (T) -> U)(x: T?) -> U? {
    return map(x, f)
}


/// Return an `Array` containing the results of mapping `transform`
/// over `source`.
func map<S : SequenceType, T>(transform: (S.Generator.Element) -> T)(source: S) -> [T] {
    return map(source, transform)
}


/// Return the greatest argument passed
func max<T : Comparable>(y: T, z: T, rest: T...)(x: T) -> T {
    return maxElement([x, y, z] + rest)
}


/// Return the greater of `x` and `y`
func max<T : Comparable>(y: T)(x: T) -> T {
    return max(x, y)
}


/// Return the lesser of `x` and `y`
func min<T : Comparable>(y: T)(x: T) -> T {
    return min(x, y)
}


/// Return the least argument passed
func min<T : Comparable>(y: T, z: T, rest: T...)(x: T) -> T {
    return minElement([x, y, z] + rest)
}


/// Returns `true` if `lhs` and `rhs` have a non-empty intersection
func overlaps<I0 : IntervalType, I1 : IntervalType where I0.Bound == I1.Bound>(rhs: I1)(lhs: I0) -> Bool {
    return overlaps(lhs, rhs)
}


/// Re-order the given `range` of `elements` and return a pivot index
/// *p*.  Postcondition: for all *i* in `range.startIndex..<`\ *p*,
/// and *j* in *p*\ `..<range.endIndex`, `less(elements[`\ *i*\ `],
/// elements[`\ *j*\ `]) && !less(elements[`\ *j*\ `], elements[`\
/// *p*\ `])`.  Only returns `range.endIndex` when `elements` is
/// empty.
/// Requires: `isOrderedBefore` is a `strict weak ordering
/// <http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings>`__
/// over `elements`.
func partition<C : MutableCollectionType where C.Index : RandomAccessIndexType>(range: Range<C.Index>, isOrderedBefore: (C.Generator.Element, C.Generator.Element) -> Bool)(inout elements: C) -> C.Index {
    return partition(&elements, range, isOrderedBefore)
}


/// Re-order the given `range` of `elements` and return a pivot index
/// *p*.  Postcondition: for all *i* in `range.startIndex..<`\ *p*,
/// and *j* in *p*\ `..<range.endIndex`, `less(elements[`\ *i*\ `],
/// elements[`\ *j*\ `]) && !less(elements[`\ *j*\ `], elements[`\
/// *p*\ `])`.  Only returns `range.endIndex` when `elements` is
/// empty.
/// Requires: The less-than operator (`func <`) defined in the `Comparable`
/// conformance is a `strict weak ordering
/// <http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings>`__
/// over `elements`.
func partition<C : MutableCollectionType where C.Index : RandomAccessIndexType, C.Generator.Element : Comparable>(range: Range<C.Index>)(inout elements: C) -> C.Index {
    return partition(&elements, range)
}


/// Return a slice, up to `maxLength` in length, containing the
/// initial elements of `s`.
///
/// If `maxLength` exceeds `countElements(s)`, the result contains all
/// the elements of `s`.
///
/// Complexity: O(1)+K when `S.Index` conforms to
/// `RandomAccessIndexType` and O(N)+K otherwise, where K is the cost
/// of slicing `s`.
func prefix<S : Sliceable>(maxLength: Int)(s: S) -> S.SubSlice {
    return prefix(s, maxLength)
}


/// Writes the textual representation of `object` into the stream `target`.
///
/// The textual representation is obtained from the `object` using its protocol
/// conformances, in the following order of preference: `Streamable`,
/// `Printable`, `DebugPrintable`.
///
/// Do not overload this function for your type.  Instead, adopt one of the
/// protocols mentioned above.
@inline(never) func print<T, TargetStream : OutputStreamType>(inout target: TargetStream)(object: T) {
    print(object, &target)
}


/// Writes the textual representation of `object` and a newline character into
/// the stream `target`.
///
/// The textual representation is obtained from the `object` using its protocol
/// conformances, in the following order of preference: `Streamable`,
/// `Printable`, `DebugPrintable`.
///
/// Do not overload this function for your type.  Instead, adopt one of the
/// protocols mentioned above.
@inline(never) func println<T, TargetStream : OutputStreamType>(inout target: TargetStream)(object: T) {
    return println(object, &target)
}


/// Return the result of repeatedly calling `combine` with an
/// accumulated value initialized to `initial` and each element of
/// `sequence`, in turn.
func reduce<S : SequenceType, U>(initial: U, @noescape combine: (U, S.Generator.Element) -> U)(sequence: S) -> U {
    return reduce(sequence, initial, combine)
}


/// Remove all elements from `x`
///
/// Invalidates all indices with respect to `x`.
///
/// :param: `keepCapacity`, if `true`, is a non-binding request to
///    avoid releasing storage, which can be a useful optimization
///    when `x` is going to be grown again.
///
/// Complexity: O(\ `countElements(x)`\ ).
func removeAll<C : RangeReplaceableCollectionType>(keepCapacity: Bool = false)(inout x: C) {
    removeAll(&x, keepCapacity: keepCapacity)
}


/// Remove from `x` and return the element at index `i`
///
/// Invalidates all indices with respect to `x`.
///
/// Complexity: O(\ `countElements(x)`\ ).
func removeAtIndex<C : RangeReplaceableCollectionType>(index: C.Index)(inout x: C) -> C.Generator.Element {
    return removeAtIndex(&x, index)
}


/// Remove from `x` the indicated `subRange` of elements
///
/// Invalidates all indices with respect to `x`.
///
/// Complexity: O(\ `countElements(x)`\ ).
func removeRange<C : RangeReplaceableCollectionType>(subRange: Range<C.Index>)(inout x: C) {
    removeRange(&x, subRange)
}


/// Sort `collection` in-place.
///
/// The sorting algorithm is not stable (can change the relative order of
/// elements that compare equal).
///
/// Requires: The less-than operator (`func <`) defined in the `Comparable`
/// conformance is a `strict weak ordering
/// <http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings>`__
/// over `elements`.
func sort<T>(isOrderedBefore: (T, T) -> Bool)(inout array: ContiguousArray<T>) {
    sort(&array, isOrderedBefore)
}

func sort<T>(isOrderedBefore: (T, T) -> Bool)(inout array: [T]) {
    sort(&array, isOrderedBefore)
}


/// Sort `collection` in-place according to `isOrderedBefore`.
///
/// The sorting algorithm is not stable (can change the relative order of
/// elements for which `isOrderedBefore` does not establish an order).
///
/// Requires: `isOrderedBefore` is a `strict weak ordering
/// <http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings>`__
/// over `elements`.
func sort<C : MutableCollectionType where C.Index : RandomAccessIndexType>(isOrderedBefore: (C.Generator.Element, C.Generator.Element) -> Bool)(inout collection: C) {
    return sort(&collection, isOrderedBefore)
}


/// Return an `Array` containing the sorted elements of `source`{according}.
///
/// The sorting algorithm is not stable (can change the relative order of
/// elements for which `isOrderedBefore` does not establish an order).
///
/// Requires: `isOrderedBefore` is a `strict weak ordering
/// <http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings>`__
/// over `elements`.
func sorted<C : SequenceType>(isOrderedBefore: (C.Generator.Element, C.Generator.Element) -> Bool)(source: C) -> [C.Generator.Element] {
    return sorted(source, isOrderedBefore)
}


/// Insert `newElements` into `x` at index `i`
///
/// Invalidates all indices with respect to `x`.
///
/// Complexity: O(\ `countElements(x) + countElements(newElements)`\ ).
func splice<C : RangeReplaceableCollectionType, S : CollectionType where C.Generator.Element == S.Generator.Element>(newElements: S, atIndex i: C.Index)(inout x: C) {
    splice(&x, newElements, atIndex: i)
}


/// Return the result of slicing `elements` into sub-sequences that
/// don't contain elements satisfying the predicate `isSeparator`.
///
/// :param: maxSplit the maximum number of slices to return, minus 1.
/// If `maxSplit + 1` slices would otherwise be returned, the
/// algorithm stops splitting and returns a suffix of `elements`
///
/// :param: allowEmptySlices if true, an empty slice is produced in
/// the result for each pair of consecutive
//func split<S : Sliceable, R : BooleanType>(elements: S, isSeparator: (S.Generator.Element) -> R, maxSplit: Int = default, allowEmptySlices: Bool = default) -> [S.SubSlice]


/// Return true iff the the initial elements of `s` are equal to `prefix`.
func startsWith<S0 : SequenceType, S1 : SequenceType where S0.Generator.Element == S1.Generator.Element, S0.Generator.Element : Equatable>(prefix: S1)(s: S0) -> Bool {
    return startsWith(s, prefix)
}


/// Return true iff `s` begins with elements equivalent to those of
/// `prefix`, using `isEquivalent` as the equivalence test.
///
/// Requires: `isEquivalent` is an `equivalence relation
/// <http://en.wikipedia.org/wiki/Equivalence_relation>`_
func startsWith<S0 : SequenceType, S1 : SequenceType where S0.Generator.Element == S1.Generator.Element>(prefix: S1, @noescape isEquivalent: (S0.Generator.Element, S1.Generator.Element) -> Bool)(s: S0) -> Bool {
    return startsWith(s, prefix, isEquivalent)
}


/// Return the sequence of values (`start`, `start + stride`, `start +
/// stride + stride`, ... *last*) where *last* is the last value in
/// the progression less than or equal to `end`.
///
/// .. Note:: There is no guarantee that `end` is an element of the
///      sequence.
func stride<T : Strideable>(through end: T, by s: T.Stride)(from start: T) -> StrideThrough<T> {
    return stride(from: start, through: end, by: s)
}


/// Return the sequence of values (`start`, `start + stride`, `start +
/// stride + stride`, ... *last*) where *last* is the last value in
/// the progression that is less than `end`.
func stride<T : Strideable>(to end: T, by s: T.Stride)(from start: T) -> StrideTo<T> {
    return stride(from: start, to: end, by: s)
}


/// Return a slice, up to `maxLength` in length, containing the
/// final elements of `s`.
///
/// If `maxLength` exceeds `countElements(s)`, the result contains all
/// the elements of `s`.
///
/// Complexity: O(1)+K when `S.Index` conforms to
/// `RandomAccessIndexType` and O(N)+K otherwise, where K is the cost
/// of slicing `s`.
func suffix<S : Sliceable where S.Index : BidirectionalIndexType>(maxLength: Int)(s: S) -> S.SubSlice {
    return suffix(s, maxLength)
}


/// Exchange the values of `a` and `b`
func swap<T>(inout b: T)(inout a: T) {
    swap(&a, &b)
}


/// Returns the the bits of `x`, interpreted as having type `U`.
///
/// .. Caution:: Breaks the guarantees of Swift's type system; use
///    with extreme care.  There's almost always a better way to do
///    anything.
///
func unsafeBitCast<T, U>(type: U.Type)(x: T) -> U {
    return unsafeBitCast(x, type)
}


/// Evaluate `f()` and return its result, ensuring that `x` is not
/// destroyed before f returns.
func withExtendedLifetime<T, Result>(@noescape f: () -> Result)(x: T) -> Result {
    return withExtendedLifetime(x, f)
}


/// Evaluate `f(x)` and return its result, ensuring that `x` is not
/// destroyed before f returns.
func withExtendedLifetime<T, Result>(@noescape f: (T) -> Result)(x: T) -> Result {
    return withExtendedLifetime(x, f)
}


/// Invokes `body` with an `UnsafeMutablePointer` to `arg` and returns the
/// result. Useful for calling Objective-C APIs that take "in/out"
/// parameters (and default-constructible "out" parameters) by pointer
func withUnsafeMutablePointer<T, Result>(@noescape body: (UnsafeMutablePointer<T>) -> Result)(inout arg: T) -> Result {
    return withUnsafeMutablePointer(&arg, body)
}


/// Like `withUnsafeMutablePointer`, but passes pointers to `arg0`, `arg1`,
/// and `arg2`.
func withUnsafeMutablePointers<A0, A1, A2, Result>(inout arg1: A1, inout arg2: A2, @noescape body: (UnsafeMutablePointer<A0>, UnsafeMutablePointer<A1>, UnsafeMutablePointer<A2>) -> Result)(inout arg0: A0) -> Result {
    return withUnsafeMutablePointers(&arg0, &arg1, &arg2, body)
}


/// Like `withUnsafeMutablePointer`, but passes pointers to `arg0` and `arg1`.
func withUnsafeMutablePointers<A0, A1, Result>(inout arg1: A1, @noescape body: (UnsafeMutablePointer<A0>, UnsafeMutablePointer<A1>) -> Result)(inout arg0: A0) -> Result {
    return withUnsafeMutablePointers(&arg0, &arg1, body)
}


/// Invokes `body` with an `UnsafePointer` to `arg` and returns the
/// result. Useful for calling Objective-C APIs that take "in/out"
/// parameters (and default-constructible "out" parameters) by pointer
func withUnsafePointer<T, Result>(@noescape body: (UnsafePointer<T>) -> Result)(inout arg: T) -> Result {
    return withUnsafePointer(&arg, body)
}


/// Like `withUnsafePointer`, but passes pointers to `arg0` and `arg1`.
func withUnsafePointers<A0, A1, Result>(inout arg1: A1, @noescape body: (UnsafePointer<A0>, UnsafePointer<A1>) -> Result)(inout arg0: A0) -> Result {
    return withUnsafePointers(&arg0, &arg1, body)
}


/// Like `withUnsafePointer`, but passes pointers to `arg0`, `arg1`,
/// and `arg2`.
func withUnsafePointers<A0, A1, A2, Result>(inout arg1: A1, inout arg2: A2, @noescape body: (UnsafePointer<A0>, UnsafePointer<A1>, UnsafePointer<A2>) -> Result)(inout arg0: A0) -> Result {
    return withUnsafePointers(&arg0, &arg1, &arg2, body)
}


/// Invoke `f` with a C `va_list` argument derived from `builder`.
func withVaList<R>(@noescape f: (CVaListPointer) -> R)(builder: VaListBuilder) -> R {
    return withVaList(builder, f)
}


/// Invoke `f` with a C `va_list` argument derived from `args`.
func withVaList<R>(@noescape f: (CVaListPointer) -> R)(args: [CVarArgType]) -> R {
    return withVaList(args, f)
}

/// A sequence of pairs built out of two underlying sequences, where
/// the elements of the `i`\ th pair are the `i`\ th elements of each
/// underlying sequence.
func zip<S0 : SequenceType, S1 : SequenceType>(s1: S1)(s0: S0) -> Zip2<S0, S1> {
    return zip(s0, s1)
}
