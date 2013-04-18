/* -*- mode: C++ -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef mozilla_tools_HeapWalk_h
#define mozilla_tools_HeapWalk_h

#include <stdint.h>

/*
 * This is a draft header for an interface not yet implemented. You can fork
 * this specification on GitHub to draft and discuss revisions:
 * [https://github.com/jimblandy/DebuggerDocs]
 *
 * Analyzing JavaScript memory consumption in Firefox is challenging because,
 * although all the objects manipulated directly by a web application are
 * JavaScript objects, those depend on a wide variety of backing data
 * structures, which use different allocation disciplines and have complex
 * internal relationships. Thus, simply discerning the form of the heap
 * graph&mdash;identifying edges, understanding which are owning references
 * and which are weak, and so on&mdash;is a daunting task, requiring detailed
 * knowlege of many different areas of Firefox.
 *
 * Even given the means to traverse this heterogenous graph, presenting the
 * results of analyses in terms useful to web developers is a further
 * challenge. While we can expect web developers to be familiar with the APIs
 * they use, many nodes in the heap graph represent internal implementation
 * details of those APIs, with which the developer has no reason to be
 * familiar. For example, from the developer's point of view, a JavaScript
 * object is a collection of named properties with values. However, at the C++
 * level, SpiderMonkey stores properties' values in anonymous, numbered
 * "slots" on the object, and then uses separate "shape" and "base shape"
 * objects to associate property names with slots. Any analysis intended for
 * use by web developers must present object-to-value edges as named
 * properties, as the developer expects, and never mention slots and shapes.
 * Similarly, Firefox represents DOM elements as JavaScript objects pointing
 * to C++ objects, with DOM parent and child links stored in the latter; but
 * analyses should present the JavaScript object and its associated C++ object
 * as a single entity.
 *
 * This header declares a C++ type, UniformNode, for traversing those parts of
 * the heap graph whose memory consumption is relevant to JavaScript
 * developers. A instance of UniformNode can point to any sort of object in
 * the heap, whether it is a JavaScript object, an XPCOM object, or some other
 * type. Given a UniformNode, one can enumerate the node's outgoing edges,
 * find edge names, and get UniformNode values for their referents. One can
 * find information about the node itself: its size in bytes, and perhaps
 * identifying information such as where it was was allocated. UniformNode
 * values compare equal when they refer to the same object; have hash values
 * that respect their equality relation; and can be serialized and
 * deserialized in a way that preserves their identity. UniformNode values are
 * small, and require no supporting data structures, making them feasible for
 * use in memory-constrained devices&mdash;ideally, the memory requirements of
 * the algorithm which uses them will be the limiting factor, not the demands
 * of UniformNode itself. Given this interface, one can implement heap
 * analysis algorithms independent of the details of the heap under study.
 * Furthermore, with appropriate changes to UniformNode's implementation,
 * Firefox's heap itself should be able to evolve without breaking existing
 * analyses.
 *
 * While UniformNode may omit non-owning or redundant edges, it generally
 * presents the heap as it actually exists in memory, and thus includes nodes
 * and edges corresponding to internal implementation details, which may have
 * no counterpart in the published specifications. To help developer tools
 * produce a view of the graph JavaScript developers would find familiar, each
 * UniformNode carries a <i>visible</i> flag, indicating whether it represents
 * an object that someone familiar with public specifications would recognize,
 * or an internal detail. Each graph edge carries a similar flag. Given these
 * annotations, a tool can present a collapsed view of the graph that includes
 * only visible nodes: starting from some visible node <i>v</i>, the tool
 * treats all visible nodes reachable from <i>v</i> by a path containing only
 * non-visible nodes as if they were <i>v</i>'s immediate neighbors. The tool
 * constructs names for these artificial edges by composing the names of
 * visible edges along the path.
 *
 * (Why must UniformNode expose non-visible nodes at all? Heap analyses
 * compute facts about the heap&mdash;for example, dominator analysis produces
 * the retained size of each node. However, these are facts about the true
 * graph; how one can best present them in the collapsed view, which includes
 * only visible nodes, depends on the analysis. Only the analysis code itself
 * can accurately and meaningfully aggregate its results. Thus, UniformNode
 * exposes the information needed to do so, but leaves the details to its
 * clients.)
 */

namespace js {
struct JSObject;
struct JSString;
struct JSScript;
class Shape;
namespace ion {
class IonCode;
}
}

namespace mozilla {

#define UNIFORMNODE_FOR_EACH_KIND(f)    \
    f(Object,    JSObject)              \
    f(String,    JSString)              \
    f(Script,    JSScript)              \
    f(Ioncode,   js::ion::IonCode)      \
    f(Shape,     js::Shape)             \
    f(BaseShape, js::BaseShape)         \
    /* end */

class UniformNode {
  public:
    enum Kind {
#define UNIFORMNODE_DECL_KIND(Name, Type) kind ## Name,
        UNIFORMNODE_FOR_EACH_KIND(UNIFORMNODE_DECL_KIND)
        kindCount
#undef UNIFORMNODE_DECL
    };

#ifdef JS_BYTES_PER_WORD == 8
  private:
    intptr_t tagged;
    static const int kind_bits = 4;
    void *pointer() const { return reinterpret_cast<void *>(tagged >> kind_bits); }

  public:
    UniformNode(void *pointer, Kind kind) {
        assert(0 <= kind && kind < kindCount);
        intptr_t address = reinterpret_cast<intptr_t>(pointer);
        tagged = (address << kind_bits) | reinterpret_cast<intptr_t>(kind);
        assert(this.pointer() == pointer);
        assert(this.kind() == kind);
    }
    UniformNode (const UniformNode &rhs) : tagged(rhs.tagged) { }
    UniformNode &operator=(const UniformNode &rhs) { tagged = rhs.tagged; }
    bool operator==(const UniformNode &rhs) const { return tagged == rhs.tagged; }
    bool operator!=(const UniformNode &rhs) const { return tagged != rhs.tagged; }
    Kind kind() const { return reinterpret_cast<Kind>(tagged & ((1 << kind_bits) - 1)); }

    /* Hash policy, suitable for use with js::HashMap and js::HashSet. */
    struct Hasher {
        typedef UniformNode Lookup;
        static js::HashNumber hash(const Lookup &l) { return l.tagged; }
        static bool match(const UniformNode &key, const Lookup &l) {
            return key == l;
        }
    };

    /*
     * UniformNode::Serialized is an integer type which can be converted to
     * and from UniformNode instances. UniformNode::deserialize(N.serialize())
     * is a node that compares and hashes equal to N.
     */
    typedef intptr_t Serialized;
  private:
    UniformNode(Serialized s) : tagged(s) { }
  public:
    Serialized serialize() const { return tagged; }
    static UniformNode deserialize(Serialized s) { return UniformNode(s); }
#else
#error "mozilla::UniformNode not implemented for target architecture"
#endif

    struct SourceLocation {
        const char *URL;
        int line, column;
    };

    struct CallStack { // Should use some extant type for this. Or at least js::Vector.
        SourceLocation *frame;
        size_t frameCount;
    };

    /*
     * Each specialization of this template represents a statically-typed
     * reference to a particular variant of UniformNode, providing reflection
     * methods on the referent, traits-like information, and so on.
     *
     * For a UniformNode referent type T, UniformNode::Variant<T> has the
     * following members:
     *
     *     typedef T referent;
     *         T, just for convenience.
     *
     *     static const Kind kind;
     *         The Kind value used for UniformNodes referring to T.
     *
     *     static const char *kindName;
     *         The name of the referent type, as a C string.
     *
     *     bool visible() const;
     *         True if this node is something that could be presented to
     *         JavaScript developers in analyses.
     *
     *     jschar *constructor()
     *         If available, the name of the constructor (JS or C++) used to
     *         create this Node's referent. For Variant<JSObject *>, this
     *         could use information saved by the JavaScript engine. For XPCOM
     *         objects, perhaps the cycle collector has some metadata. If no
     *         information is available, return NULL.
     *
     *     allocationLocation(SourceLocation *sourceLocation)
     *         If available, return the source location at which this object
     *         was allocated. Sometimes the JavaScript engine saves this data-
     *         for object and array literals, perhaps.
     *
     *     allocationCallStack(CallStack *callStack)
     *         If available, fill |*callStack| with the source location
     *         (either in JS or C++) at which this Node's referent was
     *         allocated. (If Firefox was built with refcount tracing, then I
     *         *think* that means we log call stacks for each AddRef and
     *         Release; couldn't we present the first AddRef here?)
     *
     *     class EdgeRange
     *         A class for iterating over a node's outgoing edges, modeled
     *         after js::HashTable<K,V>::Range. Each Variant<T> has its own
     *         EdgeRange class definition, but all EdgeRange classes support
     *         the following interface:
     *
     *            // True if there are no more edges in this range.
     *            bool empty() const;
     *
     *            // The name for the front edge in this range. (In real life
     *            // we'll want a better representation for edge names, to
     *            // avoid creating tons of strings when the names follow a
     *            // pattern; and we'll need to think about lifetimes
     *            // carefully to ensure traversal stays cheap.)
     *            char *frontName() const;
     *
     *            // The referent of the front edge in this range, or some
     *            // special UniformNode 'null' value if the referent is not
     *            // an allocated object (say, a numeric jsval).
     *            UniformNode frontReferent() const;
     *
     *            // Whether the front edge of this range should be visible to
     *            // JavaScript developers.
     *            bool frontVisible() const;
     *
     *         Instances of this class need not be as lightweight as
     *         UniformNode itself, since they're usually only instantiated as
     *         part of a stack frame, while iterating over a particular
     *         object's edges. For example, a dumb implementation for JS Cells
     *         might use JS_TraceChildren to to get the outgoing edges, and
     *         then store them in an array internal to the EdgeRange.
     *
     *         When used with 'match', below, template operator()
     *         instances will have the appropriate EdgeRange's member
     *         functions inlined.
     *
     * Individual specializations may provide additional members appropriate
     * to their variant.
     */
    template<typename T> class Variant {
        T *ptr;
      public:
        typedef T referent;
        Variant(T *ptr) : ptr(ptr) { }
        Variant(const UniformNode &node) : ptr(node.as<T>()) { }
        T *get() { return ptr; }
        class EdgeRange;
    };

    template<> class Variant<JSObject> {
        static const Kind kind = KindObject;
        static const char *kindName = "JSObject";
    };
    template<> class Variant<JSString> {
        static const Kind kind = KindString;
        static const char kindName = "JSString";
    };
    template<> class Variant<JSScript> {
        static const Kind kind = KindScript;
        static const char kindName = "JSScript";
    };
    template<> class Variant<js::ion::IonCode> {
        static const Kind kind = KindIonCode;
        static const char kindName = "js::ion::IonCode";
    };
    template<> class Variant<js::Shape> {
        static const Kind kind = KindShape;
        static const char kindName = "js::Shape";
    };
    template<> class Variant<js::BaseShape> {
        static const Kind kind = KindBaseShape;
        static const char kindName = "js::BaseShape";
    };

    /* is<T>() is true if this UniformNode points to a T, false otherwise. */
    template <typename T> bool is() const { return kind() == Traits<T>.kind; }

    /*
     * as<T>() returns a T * if this UniformNode points to a T, or asserts
     * otherwise.
     */
    template <typename T> T *as() const {
        assert(kind() == Traits<T>.kind);
        return reinterpret_cast<T *>(pointer());
    }

    /*
     * AsOrNull<T>() returns a T * if this UniformNode points to a T, or
     * returns NULL otherwise.
     */
    template <typename T> T *asOrNull() const {
        if (kind () == Traits<T>.kind)
            return as<T>();
        else
            return NULL;
    }

    /*
     * A strictly-typed 'switch' that supports default cases.
     *
     * Given some UniformNode N and some class C, N.match(C()) applies the
     * most specific operator() member function of C to N --- but passes N as
     * a pointer to its true type (JSObject *, js::Shape *, etc.). The 'match'
     * call returns the value C::operator() returns.
     *
     * One can define a C that has an overloading of operator() for every
     * possible kind of node, but what's more helpful is to provide
     * overloadings for some specific variants, and define a template member
     * function to handle generic cases, perhaps using UniformNode::Variant to
     * access variant-specific information. For example:
     *
     * class ClassOrTypeName {
     *     typedef const char *result;
     *     const char *operator()(JSObject *obj) {
     *         return obj->getClass()->name;
     *     }
     *     template<T>
     *     const char *operator()(T *ptr) {
     *         return UniformNode::Variant<T>::kindName;
     *     }
     * };
     *
     * Given that definition, NODE->match(ClassOrTypeName()) would evaluate to
     * a name for the type of thing NODE refers to, except in the JSObject
     * case where it returns the object's class name. ClassOrTypeName's first
     * overloading of operator() handles the JSObject case, and takes
     * advantage of knowing the exact type to call JSObject::getClass(). The
     * second overloading handles all the other UniformNode variants, and uses
     * UniformNode::Variant to pick out the right information for its variant.
     *
     * Note that this can be useful even if you only define a template member
     * function which does everything it needs to using Variant<T>: each
     * instantiation of C::operator() gets compiled separately, using the
     * appropriate types and inlined method definitions, so it's just as
     * efficient as writing out a switch with separate code for each variant.
     */
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
};

#undef UNIFORMNODE_KINDS

/*
 * Establish UniformNode::Hasher as the default hash policy for UniformNode
 * values used as keys in js::HashMap and js::HashSet objects.
 */
namespace js {
template <> struct DefaultHasher<UniformNode> : UniformNode::Hasher { };
}  // namespace js

}  // namespace mozilla

#endif // mozilla_tools_HeapWalk_h
