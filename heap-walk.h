/* -*- mode: C++ -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef mozilla_tools_HeapWalk_h
#define mozilla_tools_HeapWalk_h

#define __STDC_LIMIT_MACROS

#include <assert.h>
#include <stdint.h>

#include <js/HashTable.h>

// This is a draft header for an interface not yet implemented. You can fork
// this specification on GitHub to draft and discuss revisions:
// [https://github.com/jimblandy/DebuggerDocs]
//
// Analyzing JavaScript memory consumption in Firefox is challenging because,
// although all the objects manipulated directly by a web application are
// JavaScript objects, those depend on a wide variety of backing data
// structures, which use different allocation disciplines and have complex
// internal relationships. Thus, simply discerning the form of the heap
// graph --- identifying edges, understanding which are owning references
// and which are weak, and so on --- is a daunting task, requiring detailed
// knowlege of many different areas of Firefox.
//
// Even given the means to traverse this heterogenous graph, presenting the
// results of analyses in terms useful to web developers is a further
// challenge. While we can expect web developers to be familiar with the APIs
// they use, many nodes in the heap graph represent internal implementation
// details of those APIs, with which the developer has no reason to be
// familiar. For example, from the developer's point of view, a JavaScript
// object is a collection of named properties with values. However, at the C++
// level, SpiderMonkey stores properties' values in anonymous, numbered
// "slots" on the object, and then uses separate "shape" and "base shape"
// objects to associate property names with slots. Any analysis intended for
// use by web developers must present object-to-value edges as named
// properties, as the developer expects, and never mention slots and shapes.
// Similarly, Firefox represents DOM elements as JavaScript objects pointing
// to C++ objects, with DOM parent and child links stored in the latter; but
// analyses should present the JavaScript object and its associated C++ object
// as a single entity.
//
// This header declares a C++ type, UniformNode, for traversing those parts of
// the heap graph whose memory consumption is relevant to JavaScript
// developers. A instance of UniformNode can point to any sort of object in
// the heap, whether it is a JavaScript object, an XPCOM object, or some other
// type. Given a UniformNode, one can enumerate the node's outgoing edges,
// find edge names, and get UniformNode values for their referents. One can
// find information about the node itself: its size in bytes, and perhaps
// identifying information such as where it was was allocated. UniformNode
// values compare equal when they refer to the same object; have hash values
// that respect their equality relation; and can be serialized and
// deserialized in a way that preserves their identity. UniformNode values are
// small, and require no supporting data structures, making them feasible for
// use in memory-constrained devices --- ideally, the memory requirements of
// the algorithm which uses them will be the limiting factor, not the demands
// of UniformNode itself. Given this interface, one can implement heap
// analysis algorithms independent of the details of the heap under study.
// Furthermore, with appropriate changes to UniformNode's implementation,
// Firefox's heap itself should be able to evolve without breaking existing
// analyses.
//
// While UniformNode may omit non-owning or redundant edges, it generally
// presents the heap as it actually exists in memory, and thus includes nodes
// and edges corresponding to internal implementation details, which may have
// no counterpart in the published specifications. To help developer tools
// produce a view of the graph JavaScript developers would find familiar, each
// UniformNode carries a <i>visible</i> flag, indicating whether it represents
// an object that someone familiar with public specifications would recognize,
// or an internal detail. Each graph edge carries a similar flag. Given these
// annotations, a tool can present a collapsed view of the graph that includes
// only visible nodes: starting from some visible node <i>v</i>, the tool
// treats all visible nodes reachable from <i>v</i> by a path containing only
// non-visible nodes as if they were <i>v</i>'s immediate neighbors. The tool
// constructs names for these artificial edges by composing the names of
// visible edges along the path.
//
// In the two cases mentioned above:
//
// - A UniformNode referring to a SpiderMonkey JavaScript object presents the
//   object's slots as outgoing edges. However, when enumerating those edges,
//   UniformNode traverses the object's shape chain to label those edges as
//   JavaScript properties, thus presenting a view of the object familiar to
//   JavaScript developers.
//
// - A UniformNode referring to a JavaScript object wrapping a C++ object that
//   represents a DOM node presents the JavaScript->C++ reference as an edge,
//   and the C++ object as a separate UniformNode. However, neither that edge
//   nor the C++ object are marked as visible, directing presentation code to
//   collapse the two UniformNodes together --- again producing the view a
//   JavaScript developer expects.
//
// (Why must UniformNode expose non-visible nodes at all? Heap analyses
// compute facts about the heap --- for example, dominator analysis produces
// the retained size of each node. However, these are facts about the true
// graph; how one can best present them in the collapsed view, which includes
// only visible nodes, depends on the analysis. Only the analysis code itself
// can accurately and meaningfully aggregate its results. Thus, UniformNode
// exposes the information needed to do so, but leaves the details to its
// clients.)
//
// UniformNode has a serious limitation: it is not meant for use while the
// heap is being changed by other code. Requiring UniformNode instances to be
// registered with the collector, to allow a moving collector to update them,
// would make them infeasibly expensive. (Imagine presenting the collector
// with a large HashMap using UniformNodes as keys.) Assuming that the heap
// does not change also greatly simplifies both UniformNode's own
// implementation, and those of the algorithms that use it.
//
// Instead, analyses should use UniformNode to make a snapshot of the heap
// (either in memory or serialized), or to run the full algorithm, while other
// uses of the heap are suspended. Assuming nodes are not used by multiple
// threads, it should be sufficient to avoid running JavaScript in the
// JSRuntime being analyzed; avoid actively using XPCOM objects; and avoid
// returning to the event loop. (This means showing progress meters will be a
// challenge, if it's possible to do so at all; offering up-front estimates
// based on overall heap size may be an acceptable alternative.)
//
// (The threading question is interesting, and we need to think that through
// carefully. If I understand correctly, JavaScript objects are never shared
// between threads, other than those used internally by the engine. Web
// workers use their own JSRuntime, and inter-JSRuntime edges are not
// permitted. Background finalization threads shouldn't affect reachable
// portions of the heap, so they don't affect us. I don't think the JS engine
// has threads that muck around with reachable objects in parallel with the
// main thread, but we can check. If those assumptions are true, then simply
// not calling JavaScript and not returning to the event loop while we use
// UniformNode should suffice. I don't know if / when XPCOM objects are shared
// between threads, but I suspect that will be all right in practice, too.)

struct JSObject;
struct JSString;
struct JSScript;

namespace js {
struct RootInfo;
class Shape;
class BaseShape;
namespace ion {
class IonCode;
}
}

namespace mozilla {

#define UNIFORMNODE_FOR_EACH_KIND(f)                                          \
    f(RootInfo,  js::RootInfo)                                                \
    f(Object,    JSObject)                                                    \
    f(String,    JSString)                                                    \
    f(Script,    JSScript)                                                    \
    f(IonCode,   js::ion::IonCode)                                            \
    f(Shape,     js::Shape)                                                   \
    f(BaseShape, js::BaseShape)                                               \
    // end

class UniformNode {
  public:
    enum Kind {
#define UNIFORMNODE_DECL_KIND(Name, Type) kind ## Name,
        UNIFORMNODE_FOR_EACH_KIND(UNIFORMNODE_DECL_KIND)
        kindCount
#undef UNIFORMNODE_DECL_KIND
    };

#ifdef __x86_64__
  private:
    // This contains both the pointer, right-shifted by kind_bits, and the
    // kind, in the low bits. On x86_64, the top sixteen bits of a pointer
    // are always the sign-extension of the rest of the pointer, so an
    // arithmetic right-then-left shift preserves the pointer's value.
    intptr_t tagged;

    // How many bits at the low end of 'tagged' are reserved to hold
    // this node's kind?
    static const int kind_bits = 4;

    // Untyped constructor function. The public constructors are typed, and
    // set the node's kind appropriately.
    void init(const void *pointer, Kind kind) {
        assert(0 <= kind && kind < kindCount);
        assert(kindCount <= 1 << kind_bits);
        intptr_t address = reinterpret_cast<intptr_t>(pointer);
        tagged = (address << kind_bits) | static_cast<intptr_t>(kind);
        assert(this->pointer() == pointer);
        assert(this->kind() == kind);
    }

    // Untyped address function. The public 'as' and 'asOrNull' member
    // functions provide strictly typed access to the pointer.
    void *pointer() const { return reinterpret_cast<void *>(tagged >> kind_bits); }

  public:
    // Construct a UniformNode from a pointer to some supported variant's
    // type: JSObject *, etc.
    //
    // (At present, SpiderMonkey doesn't directly expose a JSRuntime's
    // RootInfo entries to us --- but it should. Once we have such an API,
    // then we can get UniformNodes representing each of a JSRuntime's roots,
    // and analyses can start from there if they wish.)
    template<typename T>
    UniformNode(const T *node) { init(node, Variant<T>::kind); }

    // Copy constructor and assignment operator.
    UniformNode(const UniformNode &rhs) : tagged(rhs.tagged) { }
    UniformNode &operator=(const UniformNode &rhs) { tagged = rhs.tagged; }

    // Equality, and a hash policy that respects it.
    bool operator==(const UniformNode &rhs) const { return tagged == rhs.tagged; }
    bool operator!=(const UniformNode &rhs) const { return tagged != rhs.tagged; }
    // Hash policy, suitable for use with js::HashMap and js::HashSet.
    struct Hasher {
        typedef UniformNode Lookup;
        static js::HashNumber hash(const Lookup &l) { return l.tagged; }
        static bool match(const UniformNode &key, const Lookup &l) {
            return key == l;
        }
    };

    // Return the kind of node this UniformNode represents.
    Kind kind() const { return static_cast<Kind>(tagged & ((1 << kind_bits) - 1)); }

    // is<T>() is true if this UniformNode points to a T, false otherwise.
    template<typename T>
    bool is() const { return kind() == Variant<T>::kind; }

    // as<T>() returns a T * if this UniformNode points to a T, or asserts
    // otherwise.
    template<typename T>
    T *as() const {
        assert(kind() == Variant<T>::kind);
        return reinterpret_cast<T *>(pointer());
    }

    // AsOrNull<T>() returns a T * if this UniformNode points to a T, or
    // returns NULL otherwise.
    template<typename T>
    T *asOrNull() const {
        if (kind () == Variant<T>::kind)
            return as<T>();
        else
            return NULL;
    }

    // Serialization.
    //
    // UniformNode::Serialized is an integer type which can be converted to
    // and from UniformNode instances. (We assume that the caller has its
    // own preferred ways to serialize and deserialize integers.)
    //
    // UniformNode::deserialize(N.serialize()) is a node that compares and
    // hashes equal to N.
    typedef intptr_t Serialized;
  private:
    explicit UniformNode(Serialized s) : tagged(s) { }
  public:
    Serialized serialize() const { return tagged; }
    static UniformNode deserialize(Serialized s) { return UniformNode(s); }
#else
#error "mozilla::UniformNode not implemented for target architecture"
#endif

    // Strawman definitions for source location and call stack types. Firefox
    // has existing types that play these roles, and we should use them
    // instead of defining something new.
    struct SourceLocation {
        const char *URL;
        int line, column;
    };

    struct CallStack {
        SourceLocation *frame;
        size_t frameCount;
    };

    // Each specialization of this template, UniformNode::Variant, represents
    // a statically-typed reference to a particular variant of UniformNode,
    // providing reflection methods on the referent, traits-like information,
    // and so on.
    //
    // For a UniformNode referent type T, UniformNode::Variant<T> has the
    // following members:
    //
    //     // T, just for convenience.
    //     typedef T Referent;
    //
    //     // The Kind value used for UniformNodes referring to T.
    //     static const Kind kind;
    //
    //     // The name of the referent type, as a C string.
    //     static const char *kindName;
    //
    //     // True if this node is something that could be presented to
    //     // JavaScript developers in analyses.
    //     bool visible() const;
    //
    //     // Return the compartment to which this UniformNode referent
    //     // belongs, or NULL if the object doesn't clearly belong to any
    //     // particular compartment.
    //     JSCompartment *compartment() const;
    //
    //     // If available, the name of the constructor (JS or C++) used to
    //     // create this Node's referent. For Variant<JSObject *>, this
    //     // could use information saved by the JavaScript engine. For XPCOM
    //     // objects, perhaps the cycle collector has some metadata. If no
    //     // information is available, return NULL.
    //     jschar *constructor() const;
    //
    //     // True if instances of this variant have a reference count.
    //     static const bool hasRefCount;
    //
    //     // On variants where hasRefCount is true, return the referent's
    //     // reference count. (This may be larger than the number of
    //     // incoming edges visible via UniformNode traversal, as
    //     // reference-counting isn't specified in terms of roots from
    //     // which all live objects are reachable; an object's correct
    //     // reference count is just an invariant of the code that operates
    //     // on it. An analysis might compare the number of edges observable
    //     // via UniformNode against the object's own reference count, to
    //     // infer the existence of incoming edges from objects outside the
    //     // reachable graph.) (This may be hard to implement, as reference
    //     // counts are not stored in any consistent place in XPCOM objects;
    //     // some classes don't store them at all.)
    //     int RefCount() const;
    //
    //     // If available, return the source location at which this object
    //     // was allocated. Sometimes the JavaScript engine saves this data-
    //     // for object and array literals, perhaps.
    //     allocationLocation(SourceLocation *sourceLocation) const;
    //
    //     // If available, fill |*callStack| with the source location
    //     // (either in JS or C++) at which this Node's referent was
    //     // allocated. (If Firefox was built with refcount tracing, then I
    //     // *think* that means we log call stacks for each AddRef and
    //     // Release; couldn't we present the first AddRef here?)
    //     allocationCallStack(CallStack *callStack) const;
    //
    //     // A class for iterating over a node's outgoing edges, modeled
    //     // after js::HashTable<K,V>::Range. Each Variant<T> has its own
    //     // EdgeRange class definition, but all EdgeRange classes support
    //     // the following interface:
    //     //
    //     //    // True if there are no more edges in this range.
    //     //    bool empty() const;
    //     //
    //     //    // The name for the front edge in this range. (In real life
    //     //    // we'll want a better representation for edge names, to
    //     //    // avoid creating tons of strings when the names follow a
    //     //    // pattern; and we'll need to think about lifetimes
    //     //    // carefully to ensure traversal stays cheap.)
    //     //    char *frontName() const;
    //     //
    //     //    // The referent of the front edge in this range, or some
    //     //    // special UniformNode 'null' value if the referent is not
    //     //    // an allocated object (say, a numeric jsval).
    //     //    UniformNode frontReferent() const;
    //     //
    //     //    // Whether the front edge of this range should be visible to
    //     //    // JavaScript developers.
    //     //    bool frontVisible() const;
    //     //
    //     // Instances of this class need not be as lightweight as
    //     // UniformNode itself, since they're usually only instantiated as
    //     // part of a stack frame, while iterating over a particular
    //     // object's edges. For example, a dumb implementation for JS Cells
    //     // might use JS_TraceChildren to to get the outgoing edges, and
    //     // then store them in an array internal to the EdgeRange.
    //     //
    //     // When used with 'match', below, template operator() instances
    //     // will have the appropriate EdgeRange's member functions inlined.
    //     class EdgeRange;
    //
    //     // A class with the same interface as EdgeRange, but inheriting
    //     // from UniformNode::EdgeRangeBase, and thus usable in code
    //     // not specialized for any particular variant.
    //     class DynamicEdgeRange;
    //
    // Individual specializations may provide additional members appropriate
    // to their variant. For example, some variants may have space to stash a
    // mark bit; using that where available, and HashSet<UniformNode> when not
    // could save memory when doing full graph traversal.
    //
    // C++ requires that the actual specializations be declared outside the class.
    template<typename Referent> class Variant;
    template<typename Referent> class VariantBase;

    class EdgeRangeBase;
    template<typename Referent, typename EdgeRange> class DynamicEdgeRange;

    // A strictly-typed 'switch' that supports default cases.
    //
    // Given some UniformNode N and some class C, N.match(C()) applies the
    // most specific operator() member function of C to N --- but passes N as
    // a pointer to its true type (JSObject *, js::Shape *, etc.). The 'match'
    // call returns the value C::operator() returns.
    //
    // One can define a C that has an overloading of operator() for every
    // possible kind of node, but what's more helpful is to provide
    // overloadings for some specific variants, and define a template member
    // function to handle generic cases, perhaps using UniformNode::Variant to
    // access variant-specific information. For example:
    //
    // class ClassOrTypeName {
    //     typedef const char *result;
    //     const char *operator()(JSObject *obj) {
    //         return obj->getClass()->name;
    //     }
    //     template<T>
    //     const char *operator()(T *ptr) {
    //         return UniformNode::Variant<T>::kindName;
    //     }
    // };
    //
    // Given that definition, NODE->match(ClassOrTypeName()) would evaluate to
    // a name for the type of thing NODE refers to, except in the JSObject
    // case where it returns the object's class name. ClassOrTypeName's first
    // overloading of operator() handles the JSObject case, and takes
    // advantage of knowing the exact type to call JSObject::getClass(). The
    // second overloading handles all the other UniformNode variants, and uses
    // UniformNode::Variant to pick out the right information for its variant.
    //
    // Note that this can be useful even if you only define a template member
    // function which does everything it needs to using Variant<T>: each
    // instantiation of C::operator() gets compiled separately, using the
    // appropriate types and inlined method definitions, so it's just as
    // efficient as writing out a switch with separate code for each variant.
    template<typename C>
    typename C::result match(const C &c) const {
        switch (kind()) {
#define UNIFORMNODE_MATCH_CASE(Name, Type)                                    \
          case kind ## Name: return c(as<Type>());
          UNIFORMNODE_FOR_EACH_KIND(UNIFORMNODE_MATCH_CASE)
#undef UNIFORMNODE_MATCH_CASE
          default: abort();
        }
    }

    // UniformNode::match case class for dynamicEdgeRange. (Ideally, this
    // class would be local to dynamicEdgeRange, but local classes can't
    // have member templates.)
    struct MakeEdgeRange {
        typedef EdgeRangeBase *result;
        template<typename T>
        EdgeRangeBase *operator()(T *ptr) const {
            return new typename UniformNode::Variant<T>::DynamicEdgeRange(ptr);
        }
    };

    // Return an EdgeRangeBase implementation appropriate for this node's
    // referent.
    EdgeRangeBase *dynamicEdgeRange();
    // Definition commented out, because it requires full definitions of
    // all variants and their EdgeRange types. But here's the full
    // definition:
    // { return match(MakeEdgeRange()); }
};

#undef UNIFORMNODE_FOR_EACH_KIND

// Base class for variant classes.
template<typename Referent>
class UniformNode::VariantBase {
    Referent *ptr;
  public:
    VariantBase(Referent *ptr) : ptr(ptr) { }
    VariantBase(const UniformNode &node) : ptr(node.as<Referent>()) { }
    operator UniformNode() const { return UniformNode(ptr); }
    Referent *get() const { return ptr; }
};

// An EdgeRange abstract base class, supporting the same interface as the
// variant-specific EdgeRange classes, except that the member functions are
// virtual. Each Variant<T>::DynamicEdgeRange class inherits from this base
// class.
class UniformNode::EdgeRangeBase {
  public:
    virtual bool empty() const = 0;
    virtual char *frontName() const = 0;
    virtual UniformNode frontReferent() const = 0;
    virtual bool frontVisible() const = 0;
};

// Template for DynamicEdgeRange implementations.
template<typename Referent, typename EdgeRange>
class UniformNode::DynamicEdgeRange : public EdgeRangeBase {
    EdgeRange concrete;
  public:
    explicit DynamicEdgeRange(Referent *node): concrete(node) { }
    bool empty() const                  { return concrete.empty(); }
    char *frontName() const             { return concrete.frontName(); }
    UniformNode frontReferent() const   { return concrete.frontReferent(); }
    bool frontVisible() const           { return concrete.frontVisible(); }
};

// Specializations of UniformNode::Variant for each variant type that we
// support.
//
// These could be placed in separate header files, but all of those would
// need to be #included in compilation units which use UniformNode::match,
// since that wants static information about each variant's members, member
// types, and so on.

template<>
class UniformNode::Variant<js::RootInfo> : public UniformNode::VariantBase<js::RootInfo> {
  public:
    typedef js::RootInfo Referent;
    static const Kind kind = kindRootInfo;
    static const char *kindName;
    class EdgeRange;
    typedef UniformNode::DynamicEdgeRange<Referent, EdgeRange> DynamicEdgeRange;
};

template<>
class UniformNode::Variant<JSObject> : public UniformNode::VariantBase<JSObject> {
  public:
    typedef JSObject Referent;
    static const Kind kind = kindObject;
    static const char *kindName;
    class EdgeRange;
    typedef UniformNode::DynamicEdgeRange<Referent, EdgeRange> DynamicEdgeRange;
};

template<>
class UniformNode::Variant<JSString> : public UniformNode::VariantBase<JSString> {
  public:
    typedef JSString Referent;
    static const Kind kind = kindString;
    static const char kindName;
    class EdgeRange;
    typedef UniformNode::DynamicEdgeRange<Referent, EdgeRange> DynamicEdgeRange;
};

template<>
class UniformNode::Variant<JSScript> : public UniformNode::VariantBase<JSScript> {
  public:
    typedef JSScript Referent;
    static const Kind kind = kindScript;
    static const char kindName;
    class EdgeRange;
    typedef UniformNode::DynamicEdgeRange<Referent, EdgeRange> DynamicEdgeRange;
};

template<>
class UniformNode::Variant<js::ion::IonCode> : public UniformNode::VariantBase<js::ion::IonCode> {
  public:
    typedef js::ion::IonCode Referent;
    static const Kind kind = kindIonCode;
    static const char kindName;
    class EdgeRange;
    typedef UniformNode::DynamicEdgeRange<Referent, EdgeRange> DynamicEdgeRange;
};

template<>
class UniformNode::Variant<js::Shape> : public UniformNode::VariantBase<js::Shape> {
  public:
    typedef js::Shape Referent;
    static const Kind kind = kindShape;
    static const char kindName;
    class EdgeRange;
    typedef UniformNode::DynamicEdgeRange<Referent, EdgeRange> DynamicEdgeRange;
};

template<>
class UniformNode::Variant<js::BaseShape> : public UniformNode::VariantBase<js::BaseShape> {
  public:
    typedef js::BaseShape Referent;
    static const Kind kind = kindBaseShape;
    static const char kindName;
    class EdgeRange;
    typedef UniformNode::DynamicEdgeRange<Referent, EdgeRange> DynamicEdgeRange;
};

}  // namespace mozilla

// Establish UniformNode::Hasher as the default hash policy for UniformNode
// values used as keys in js::HashMap and js::HashSet objects.
namespace js {
template<> struct DefaultHasher<mozilla::UniformNode> : mozilla::UniformNode::Hasher { };
}  // namespace js

#endif // mozilla_tools_HeapWalk_h
