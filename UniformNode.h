/* -*- mode: C++ -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef vm_UniformNode_h
#define vm_UniformNode_h

#define __STDC_LIMIT_MACROS

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>

#include "jspubtd.h"

#include "js/Utility.h"
#include "mozilla/Vector.h"
#include "mozilla/Assertions.h"
#include "mozilla/Maybe.h"
#include "mozilla/Move.h"
#include "mozilla/PodOperations.h"

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
// This header declares a C++ type, js::uniformnode::Node, for traversing those parts of
// the heap graph whose memory consumption is relevant to JavaScript
// developers. A instance of Node can point to any sort of object in
// the heap, whether it is a JavaScript object, an XPCOM object, or some other
// type. Given a Node, one can enumerate the node's outgoing edges,
// find edge names, and get Node values for their referents. One can
// find information about the node itself: its size in bytes, and perhaps
// identifying information such as where it was was allocated. Node
// values compare equal when they refer to the same object; have hash values
// that respect their equality relation; and can be serialized and
// deserialized in a way that preserves their identity. Node values are
// small, and require no supporting data structures, making them feasible for
// use in memory-constrained devices --- ideally, the memory requirements of
// the algorithm which uses them will be the limiting factor, not the demands
// of Node itself. Given this interface, one can implement heap
// analysis algorithms independent of the details of the heap under study.
// Furthermore, with appropriate changes to Node's implementation,
// Firefox's heap itself should be able to evolve without breaking existing
// analyses.
//
// While Node may omit non-owning or redundant edges, it generally
// presents the heap as it actually exists in memory, and thus includes nodes
// and edges corresponding to internal implementation details, which may have
// no counterpart in the published specifications. To help developer tools
// produce a view of the graph JavaScript developers would find familiar, each
// Node carries a <i>visible</i> flag, indicating whether it represents
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
// - A Node referring to a SpiderMonkey JavaScript object presents the
//   object's slots as outgoing edges. However, when enumerating those edges,
//   Node traverses the object's shape chain to label those edges as
//   JavaScript properties, thus presenting a view of the object familiar to
//   JavaScript developers.
//
// - A Node referring to a JavaScript object wrapping a C++ object that
//   represents a DOM node presents the JavaScript->C++ reference as an edge,
//   and the C++ object as a separate Node. However, neither that edge
//   nor the C++ object are marked as visible, directing presentation code to
//   collapse the two UniformNodes together --- again producing the view a
//   JavaScript developer expects.
//
// (Why must Node expose non-visible nodes at all? Heap analyses
// compute facts about the heap --- for example, dominator analysis produces
// the retained size of each node. However, these are facts about the true
// graph; how one can best present them in the collapsed view, which includes
// only visible nodes, depends on the analysis. Only the analysis code itself
// can accurately and meaningfully aggregate its results. Thus, Node
// exposes the information needed to do so, but leaves the details to its
// clients.)
//
// Node has a serious limitation: it is not meant for use while the
// heap is being changed by other code. Requiring Node instances to be
// registered with the collector, to allow a moving collector to update them,
// would make them much more expensive. (Imagine presenting the collector
// with a large HashMap using UniformNodes as keys.) Assuming that the heap
// does not change also greatly simplifies both Node's own
// implementation, and those of the algorithms that use it.
//
// Instead, analyses should use Node to make a snapshot of the heap
// (either in memory or serialized), or to run the full algorithm, while
// other uses of the heap are suspended. Assuming nodes are not used by
// multiple threads, it should be sufficient to avoid running JavaScript in
// the JSRuntime being analyzed; avoid actively using XPCOM objects; and
// avoid returning to the event loop. (This means showing progress meters
// will be a challenge, if it's possible to do so at all; offering up-front
// time estimates based on overall heap size may be an acceptable
// alternative, or forking and doing the analysis on the child process.)
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
// Node should suffice. I don't know if / when XPCOM objects are shared
// between threads, but I suspect that will be all right in practice, too.)

struct JSObject;
struct JSString;
struct JSScript;

namespace js {
class Shape;
class BaseShape;
class LazyScript;
class RootInfo;
namespace ion {
class IonCode;
}
namespace types {
class TypeObject;
}

namespace uniformnode {

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

// NonObject is the dummy referent type for UniformNodes that we produce for
// jsvals that don't refer to anything in memory (booleans; integers). We leave
// the class incomplete. Having this allows us to say that every edge has a
// referent, and not have to skip enumerating jsval edges based on the jsval
// type.
class NonObject;

#define UNIFORMNODE_FOR_EACH_KIND(f)                                          \
    f(NonObject,  NonObject)                                                  \
    f(Runtime,    JSRuntime)                                                  \
    f(Object,     JSObject)                                                   \
    f(String,     JSString)                                                   \
    f(Script,     JSScript)                                                   \
    f(LazyScript, js::LazyScript)                                             \
    f(IonCode,    js::ion::IonCode)                                           \
    f(Shape,      js::Shape)                                                  \
    f(BaseShape,  js::BaseShape)                                              \
    f(TypeObject, js::types::TypeObject)                                      \
    // end

// The tag values we use in the lower bits of a UniformNode.
enum Kind {
#define UNIFORMNODE_DECL_KIND(Name, Type) kind ## Name,
    UNIFORMNODE_FOR_EACH_KIND(UNIFORMNODE_DECL_KIND)
    kindCount
#undef UNIFORMNODE_DECL_KIND
};

// For each Node referent type T (JSObject, Shape, etc.), a
// Variant<T> instance refers to a particular T, providing
// reflection methods on its referent, traits-like information, and so on.
//
// The VariantExample class, below, documents the members each Variant
// specialization is expected to have.
//
// The VariantFinisher class template, below, provides default definitions for
// some members that Variant specializations must have; it may be helpful in
// defining specializations.
//
// Individual specializations may provide additional members appropriate
// to their variant. For example, some variants may have space to stash a
// mark bit; using that where available, and HashSet<Node> when not
// could save memory when doing full graph traversal.
template<typename Referent> class Variant;

// All these are defined later.
template<typename Base> class VariantFinisher;
class VariantBase;
class DynamicEdge;
class DynamicEdgeRange;

class Node {
  private:
    // This contains both the pointer, right-shifted by kind_bits, and the
    // kind, in the low bits. On x86_64, the top sixteen bits of a pointer
    // are always the sign-extension of the rest of the pointer, so an
    // arithmetic right-then-left shift preserves the pointer's value.
    // Perhaps a tighter representation could be found on 32-bit machines.
    int64_t tagged;

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
    Node() { init(NULL, kindNonObject); }
    // Construct a Node from a pointer to some supported variant's
    // type: JSObject *, etc.
    template<typename T>
    Node(const T *node) { init(node, Variant<T>::kind); }
    Node(void *, JSGCTraceKind);

    // Return the UniformNode representing the runtime, whose outgoing edges are
    // the runtime's roots.
    Node(JSRuntime *rt);

    // Copy constructor and assignment operator.
    Node(const Node &rhs) : tagged(rhs.tagged) { }
    Node &operator=(const Node &rhs) { tagged = rhs.tagged; return *this; }

    // Equality, and a hash policy that respects it.
    bool operator==(const Node &rhs) const { return tagged == rhs.tagged; }
    bool operator!=(const Node &rhs) const { return tagged != rhs.tagged; }
    // Hash policy, suitable for use with js::HashMap and js::HashSet.
    struct Hasher {
        typedef Node Lookup;
        static js::HashNumber hash(const Lookup &l) { return l.tagged; }
        static bool match(const Node &key, const Lookup &l) {
            return key == l;
        }
    };

    // Return the kind of node this Node represents.
    Kind kind() const { return static_cast<Kind>(tagged & ((1 << kind_bits) - 1)); }

    // is<T>() is true if this Node points to a T, false otherwise.
    template<typename T>
    bool is() const { return kind() == Variant<T>::kind; }

    // as<T>() returns a T * if this Node points to a T, or asserts
    // otherwise.
    template<typename T>
    T *as() const {
        assert(kind() == Variant<T>::kind);
        return reinterpret_cast<T *>(pointer());
    }

    // AsOrNull<T>() returns a T * if this Node points to a T, or
    // returns NULL otherwise.
    template<typename T>
    T *asOrNull() const {
        if (kind () == Variant<T>::kind)
            return as<T>();
        else
            return NULL;
    }

    // A Node is true if the address it holds is non-NULL.
    operator bool() { return !!pointer(); }

    // Serialization.
    //
    // Node::Serialized is an integer type which can be converted to
    // and from Node instances. (We assume that the caller has its
    // own preferred ways to serialize and deserialize integers.)
    //
    // Node::deserialize(N.serialize()) is a node that compares and
    // hashes equal to N.
    typedef intptr_t Serialized;
  private:
    explicit Node(Serialized s) : tagged(s) { }
  public:
    Serialized serialize() const { return tagged; }
    static Node deserialize(Serialized s) { return Node(s); }

    // A strictly-typed 'switch' that supports default cases.
    //
    // Given some Node N and some class C, N.match(C()) applies the
    // most specific operator() member function of C to N --- but passes N as
    // a pointer to its true type (JSObject *, js::Shape *, etc.). The 'match'
    // call returns whatever the call to C::operator() returns.
    //
    // One can define a C that has an overloading of operator() for every
    // possible kind of node, but what's more helpful is to provide
    // overloadings for some specific variants, and define a template member
    // function to handle generic cases, perhaps using Variant to
    // access variant-specific information. For example:
    //
    // class ClassOrTypeName {
    //     typedef const char *Result;
    //     const char *operator()(JSObject *obj) {
    //         return obj->getClass()->name;
    //     }
    //     template<T>
    //     const char *operator()(T *ptr) {
    //         return Variant<T>::kindName;
    //     }
    // };
    //
    // Given that definition, NODE->match(ClassOrTypeName()) would evaluate to
    // a name for the type of thing NODE refers to, except in the JSObject
    // case where it returns the object's class name. ClassOrTypeName's first
    // overloading of operator() handles the JSObject case, and takes
    // advantage of knowing the exact type to call JSObject::getClass(). The
    // second overloading handles all the other Node variants, and uses
    // Variant to pick out the right information for its variant.
    //
    // Note that this can be useful even if you only define a template member
    // function which does everything it needs to using Variant<T>: each
    // instantiation of C::operator() gets compiled separately, using the
    // appropriate types and inlined method definitions, so it's just as
    // efficient as writing out a switch with separate code for each variant.
    template<typename C>
    typename C::Result match(const C &c) const {
        switch (kind()) {
#define UNIFORMNODE_MATCH_CASE(Name, Type) \
            case kind ## Name: return c(as<Type>());
            UNIFORMNODE_FOR_EACH_KIND(UNIFORMNODE_MATCH_CASE)
#undef UNIFORMNODE_MATCH_CASE
          default: abort();
        }
    }

    // Metadata methods that dispatch based on type; see the corresponding
    // methods in VariantExample for documentation.

    // The name of the referent type, as a (statically allocated) C string.
    const char *kindName() const;

    // True if this kind of node is something that JavaScript developers know
    // about, and that is described in public specifications, or has some
    // counterpart that is. False if it's an internal implementation detail.
    bool visible() const;

    // Return the compartment to which the referent belongs, or NULL if the
    // object doesn't clearly belong to any particular compartment.
    JSCompartment *compartment() const;

    // If available, the name of the constructor (JS or C++) used to create this
    // Node's referent. For Variant<JSObject *>, this could use information
    // saved by the JavaScript engine. For XPCOM objects, perhaps the cycle
    // collector has some metadata. If no information is available, return NULL.
    jschar *constructor() const;

    // True if instances of this variant have a reference count.
    bool hasRefCount() const;

    // On variants where hasRefCount is true, return the referent's
    // reference count.
    //
    // (Note that this may be larger than the number of incoming edges
    // visible via Node traversal, as reference-counting doesn't
    // work in terms of roots, from which all live objects are reachable;
    // an object's correct reference count is just an invariant of the
    // code that operates on it. An analysis might compare the number of
    // edges observable via Node against the object's own
    // reference count, to infer the existence of incoming edges from
    // objects outside the reachable graph.)
    //
    // (This may be hard to implement, as reference counts are not stored
    // in any consistent place in XPCOM objects; some classes don't store
    // them at all.)
    int RefCount() const;

    // If available, fill *sourceLocation with the source location at
    // which this object was allocated. Sometimes the JavaScript engine
    // saves this data.
    void allocationLocation(SourceLocation *sourceLocation) const;

    // If available, fill |*callStack| with the source location
    // (either in JS or C++) at which this Node's referent was
    // allocated. (If Firefox was built with refcount tracing, then I
    // *think* that means we log call stacks for each AddRef and
    // Release; couldn't we present the first AddRef here?)
    void allocationCallStack(CallStack *callStack) const;

    // Return a DynamicEdgeRange implementation appropriate for this node's
    // referent. The caller is responsible for deleting the returned
    // instance. Return NULL on error.
    DynamicEdgeRange *dynamicEdgeRange(JSContext *cx) const;
};

// For each referent type T, Variant<T>::DynamicEdge and
// Variant<T>::DynamicEdgeRange implement these DynamicEdge and DynamicEdgeRange
// classes, which provide interfaces very similar to Edge and EdgeRange, but use
// virtual function dispatch, and are thus usable in code not specialized for
// any particular variant.
class DynamicEdge {
  public:
    virtual ~DynamicEdge();
    virtual const char *name() const = 0;
    virtual Node referent() const = 0;
    virtual bool visible() const = 0;
};

class DynamicEdgeRange {
  public:
    virtual ~DynamicEdgeRange();
    virtual bool empty() const = 0;
    virtual const DynamicEdge &front() const = 0;
    virtual void popFront() = 0;
};

// An example class for Variant<T> specializations. Those which are named after
// methods of Node are the variant-specific implementations for those methods.
//
// (Variant<T> specializations shouldn't actually use this as a base class: much
// of what we want from each Variant<T> is types and constants, so it's not very
// useful to refer to a Variant<T> instance via some common base type. This is
// simply documentation: we need to explain what the variant members mean, and
// we might as well put it in a form the compiler can check for basic sanity.)
class VariantExample {
  public:
    // The referent type, just for convenience.
    typedef char /* whatever */ Referent;

    // The Kind value used for UniformNodes referring to T.
    static const Kind kind;

    // The name of the referent type, as a C string.
    static const char *kindName;

    // Return this variant's referent.
    Referent &ref();

    // Variant-specific implementations of Node methods.
    // See their descriptions in Node.
    bool visible() const;
    JSCompartment *compartment() const;
    jschar *constructor() const;
    static const bool hasRefCount = false;
    int RefCount() const;
    void allocationLocation(SourceLocation *sourceLocation) const;
    void allocationCallStack(CallStack *callStack) const;

    // Below are a bunch of base classes for member types each Variant<T>
    // must provide. Each VariantExample::Foo class here is the base class for
    // Variant<T>::Foo. (Hopefully the "-Base" in "VariantExample" makes it
    // clear that these are abstract base classes.)

    // For each referent type T, Variant<T>::Edge should be a class
    // representing an outgoing edge of a node. Edges are owned by
    // EdgeRanges, and need not have assignment operators or copy
    // constructors.
    //
    // Each Edge class should inherit from this base class, overriding as
    // appropriate.
    class Edge {
        // This edge's name.
        //
        // (In real life we'll want a better representation for names, to
        // avoid creating tons of strings when the names follow a pattern;
        // and we'll need to think about lifetimes carefully to ensure
        // traversal stays cheap.)
        const char *name() const;

        // This edge's referent.
        Node referent() const;

        // Whether the front edge of this range should be visible to
        // JavaScript developers.
        bool visible() const;
    };

    // For each referent type T, Variant<T>::EdgeRange should be a class
    // for iterating over a node's outgoing edges, inheriting from this
    // class. (This is modeled after js::HashTable<K,V>::Range.)
    //
    // Instances of this class need not be as lightweight as
    // Node itself, since they're usually only instantiated as
    // part of a stack frame, while iterating over a particular
    // object's edges. For example, a dumb implementation for JS Cells
    // might use JS_TraceChildren to to get the outgoing edges, and
    // then store them in an array internal to the EdgeRange.
    //
    // When used with 'match', below, template operator() instances
    // will have the appropriate EdgeRange's member functions inlined.
    class EdgeRange {
        // Initialize this EdgeRange to represent all the outgoing edges from
        // |node|. This may only be called once on a given EdgeRange instance.
        // Return true on success, false on error.
        bool init(JSContext *cx, const Referent *node);

        // True if there are no more edges in this range.
        bool empty() const;

        // The front edge of this range.
        const Edge &front() const;

        // Remove the front edge from this range. This should only be called if
        // !empty().
        void popFront();
    };
};

// Variant specializations need a lot of boilerplate. We have two classes to
// help provide it: VariantBase is a base class for all specializations, and
// VariantFinisher is a template that takes a base class and finishes it off.
//
// So the whole definition of a variant for a referent type Foo is a big fat C++
// sandwich with rich templatey seasoning and a center filling of your own
// creation, that looks like this:
//
// class FooSpecifics : public VariantBase { ... interesting specifics ... };
// template<> class Variant<Foo> : public VariantFinisher<FooSpecifics> {
//    Variant(Foo *ptr) : VariantFinisher<FooSpecifics>(ptr) { } // dumb
// };


// A base class for Variant specializations.
//
// It needs to be easy to invent a new reflection method for Node and then
// gradually implement it for those variants on which it makes sense. It
// certainly should not be necessary to go through every Variant specialization
// and add a dummy "info not available" version of the method. Instead, we have
// all specializations inherit from VariantBase here, put the dummy definition
// in VariantBase (or a definition we expect to be useful to most variants), and
// add more interesting definitions to the specializations that can use them.
struct VariantBase {
    // These are attempts to provide defaults that will often be correct; you
    // can override them.
    bool visible() const { return true; }
    JSCompartment *compartment() const { return NULL; }
    jschar *constructor() const { return NULL; }
    static const bool hasRefCount = false;
    int RefCount() const {
        MOZ_ASSUME_UNREACHABLE("Called Variant<T>::RefCount,"
                               " but T has no reference count");
    }

    void allocationLocation(SourceLocation *sourceLocation) const {
        mozilla::PodZero(sourceLocation);
    }

    void allocationCallStack(CallStack *callStack) const {
        mozilla::PodZero(callStack);
    }
};


// A class template that takes care of defining boilerplate for a Variant, given
// a base class that defines the essentials.
//
// If Base is a class that provides a basic description of the variant, then
// VariantFinisher<Base> is a class that inherits from Base and provides various
// boilerplate definitions: some constructors and methods; DynamicEdge and
// DynamicEdgeRange classes; and anything else we can figure out how to define.
//
// VariantFinisher uses the following members of Base:
// - Referent, Edge, EdgeRange: as described in VariantExample
// - ptr: a pointer to the referent.
template<typename Base>
class VariantFinisher : public Base {
    typedef typename Base::Referent Referent;
    typedef typename Base::Edge Edge;
    typedef typename Base::EdgeRange EdgeRange;

  public:
    VariantFinisher(Referent *ptr) : Base(ptr) { }
    VariantFinisher(const Node &node) : Base(node.as<Referent>()) { }
    operator Node() const { return Node(Base::ptr); }

    Referent &ref() const { return Base::ptr; }

    class DynamicEdgeRange;

    class DynamicEdge : public uniformnode::DynamicEdge {
        friend DynamicEdgeRange;
        const Edge *concrete;
        DynamicEdge() : concrete(NULL) { }
        void set(const Edge *concrete) { this->concrete = concrete; }

      public:
        virtual const char *name() const { return concrete->name(); }
        virtual Node referent() const { return concrete->referent(); }
        virtual bool visible() const { return concrete->visible(); }
    };

    class DynamicEdgeRange : public uniformnode::DynamicEdgeRange {
        EdgeRange concrete;

        // Given the lifetime restrictions on DynamicEdgeRange::front, we only
        // need this single DynamicEdge; we just re-point it to each concrete
        // edge we get as we traverse.
        mutable DynamicEdge edge;

      public:
        bool init(JSContext *cx, Referent *ptr) { return concrete.init(cx, ptr); }
        virtual bool empty() const { return concrete.empty(); }
        virtual const DynamicEdge &front() const {
            edge.set(&concrete.front());
            return edge;
        }
        virtual void popFront() { concrete.popFront(); }
    };
};

// A dumb edge class, for edge ranges that simply produce a vector of all the
// edges and hand them out.
class SimpleEdge {
    char *name_;
    Node referent_;

    SimpleEdge(SimpleEdge &) MOZ_DELETE;
  public:
    SimpleEdge() : name_(NULL) { }
    SimpleEdge(char *name, const Node &referent) : name_(name), referent_(referent) { }
    ~SimpleEdge() { free(name_); }

    // Some move constructors, in both the new style (because Move.h says we
    // should) and the old style (because Vector.h uses it internally).
    SimpleEdge(SimpleEdge &&rhs) : name_(rhs.name_), referent_(rhs.referent_) {
        rhs.name_ = NULL;
    }
    SimpleEdge(mozilla::MoveRef<SimpleEdge> rhs) : name_(rhs->name_), referent_(rhs->referent_) {
        rhs->name_ = NULL;
    }
    SimpleEdge &operator=(SimpleEdge &&rhs) {
        this->~SimpleEdge();
        new(this) SimpleEdge(mozilla::Move(rhs));
        return *this;
    }

    const char *name() const { return name_; }
    Node referent() const { return referent_; }
    bool visible() const { return true; }
};


// A dumb EdgeRange base class that just has a vector of Edges, populated by the
// init method. The derived class must supply the actual init implementation.
template<typename Edge>
class VectorEdgeRange {
  protected:
    mozilla::Vector<Edge> edges;
    size_t i;
    bool append(Edge &&edge) { return edges.append(edge); }

  public:
    bool empty() const { return i >= edges.length(); }
    const SimpleEdge &front() const { return edges[i]; }
    void popFront() { i++; }
};


// Specializations of Variant for each variant type that we support.
//
// These could be placed in separate header files, but all of those would need
// to be #included in compilation units which use Node::match, since that wants
// static information about each variant's members, member types, and so on.

struct NonObjectDetails : public VariantBase {
    typedef NonObject Referent;
    static const Kind kind = kindNonObject;
    static const char *kindName;

    NonObject *ptr; // required by VariantFinisher, but dumb

    NonObjectDetails(NonObject *ptr) : ptr(ptr) { }

    struct Edge {
        const char *name() const { MOZ_ASSUME_UNREACHABLE("Node NonObjects have no edges"); }
        Node referent() const { MOZ_ASSUME_UNREACHABLE("Node NonObjects have no edges"); }
        bool visible() const { MOZ_ASSUME_UNREACHABLE("Node NonObjects have no edges"); }
    };

    struct EdgeRange {
        bool init(JSContext *cx, const NonObject *node) { return true; }
        bool empty() const { return true; }
        const Edge &front() const { MOZ_ASSUME_UNREACHABLE("Node NonObjects have no edges"); }
        void popFront() { MOZ_ASSUME_UNREACHABLE("Node NonObjects have no edges"); }
    };
};

template<>
struct Variant<NonObject> : public VariantFinisher<NonObjectDetails> {
    Variant(NonObject *ptr) : VariantFinisher<NonObjectDetails>(ptr) { }
};

struct RuntimeDetails : public VariantBase {
    typedef JSRuntime Referent;
    static const Kind kind = kindRuntime;
    static const char *kindName;

    RuntimeDetails(JSRuntime *ptr) { }

    typedef SimpleEdge Edge;
    class EdgeRange : public VectorEdgeRange<Edge> {
      public:
        bool init(JSContext *cx, const JSRuntime *node);
    };
};

template<>
struct Variant<JSRuntime> : public VariantFinisher<RuntimeDetails> {
    Variant(JSRuntime *ptr) : VariantFinisher<RuntimeDetails>(ptr) { }
};

// Implement edge enumeration for the following variants using JS_TraceChildren.
//
// JS_TraceChildren doesn't tell us much about the edges; We'll certainly want
// to provide more metadata about edges eventually. But this is a first cut.
#define UNIFORMNODE_FOR_EACH_JSTRACER_KIND(f)                                 \
    f(Object,     JSObject,              JSTRACE_OBJECT)                      \
    f(String,     JSString,              JSTRACE_STRING)                      \
    f(Script,     JSScript,              JSTRACE_SCRIPT)                      \
    f(LazyScript, js::LazyScript,        JSTRACE_LAZY_SCRIPT)                 \
    f(IonCode,    js::ion::IonCode,      JSTRACE_IONCODE)                     \
    f(Shape,      js::Shape,             JSTRACE_SHAPE)                       \
    f(BaseShape,  js::BaseShape,         JSTRACE_BASE_SHAPE)                  \
    f(TypeObject, js::types::TypeObject, JSTRACE_TYPE_OBJECT)                 \
    // end

// Common base class for all JSTracer-based EdgeRange implementations.
class JSTracerEdgeRange : public VectorEdgeRange<SimpleEdge> {
    bool init(JSContext *cx, void *thing, JSGCTraceKind kind);
  public:
    // Overloading the constructor allows us to be used directly as the
    // EdgeRange class for all the JSTracer-based variants.
    bool init(JSContext *cx, JSObject *ptr) { return init(cx, ptr, JSTRACE_OBJECT); }
    bool init(JSContext *cx, JSString *ptr) { return init(cx, ptr, JSTRACE_STRING); }
    bool init(JSContext *cx, JSScript *ptr) { return init(cx, ptr, JSTRACE_SCRIPT); }
    bool init(JSContext *cx, js::LazyScript *ptr) { return init(cx, ptr, JSTRACE_LAZY_SCRIPT); }
    bool init(JSContext *cx, js::ion::IonCode *ptr) { return init(cx, ptr, JSTRACE_IONCODE); }
    bool init(JSContext *cx, js::Shape *ptr) { return init(cx, ptr, JSTRACE_SHAPE); }
    bool init(JSContext *cx, js::BaseShape *ptr) { return init(cx, ptr, JSTRACE_BASE_SHAPE); }
    bool init(JSContext *cx, js::types::TypeObject *ptr) {
        return init(cx, ptr, JSTRACE_TYPE_OBJECT);
    }
};

#define UNIFORMNODE_DEFINE_DETAILS(Name, Type, TracerEnum)                    \
    struct Name ## Details : public VariantBase {                             \
        typedef Type Referent;                                                \
        static const Kind kind = kind ## Name;                                \
        static const char *kindName;                                          \
                                                                              \
        Referent *ptr;                                                        \
        Name ## Details(Referent *ptr) : ptr(ptr) { }                         \
                                                                              \
        typedef SimpleEdge Edge;                                              \
        typedef JSTracerEdgeRange EdgeRange;                                  \
    };                                                                        \
                                                                              \
    template<>                                                                \
    struct Variant<Type> : public VariantFinisher<Name ## Details> {           \
        Variant(Type *ptr) : VariantFinisher<Name ## Details>(ptr) { }        \
    };
UNIFORMNODE_FOR_EACH_JSTRACER_KIND(UNIFORMNODE_DEFINE_DETAILS)
#undef UNIFORMNODE_DEFINE_DETAILS

}  // namespace uniformnode

// Establish Node::Hasher as the default hash policy for Node
// values used as keys in js::HashMap and js::HashSet objects.
template <class> struct DefaultHasher;
  template<> struct DefaultHasher<uniformnode::Node> : uniformnode::Node::Hasher { };
}  // namespace js

#endif // vm_UniformNode_h
