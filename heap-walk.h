/* -*- mode: C++ -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef mozilla_tools_HeapWalk_h
#define mozilla_tools_HeapWalk_h

#include <stdint.h>

/*
 * This is a draft header for an interface not yet implemented. You can
 * fork this specification on GitHub to draft and discuss revisions:
 * [https://github.com/jimblandy/DebuggerDocs]
 *
 * Analyzing JavaScript memory consumption in Firefox is challenging
 * because, although all the objects manipulated directly by a web
 * application are JavaScript objects, those depend on a wide variety of
 * backing data structures, which use different allocation disciplines and
 * have complex internal relationships. Thus, simply discerning the form of
 * the heap graph&mdash;identifying edges, understanding which are owning
 * references and which are weak, and so on&mdash;is a daunting task,
 * requiring detailed knowlege of many different areas of Firefox.
 *
 * Even given the means to traverse this heterogenous graph, presenting the
 * results of analyses in terms useful to web developers is a further
 * challenge. While we can expect web developers to be familiar with the
 * APIs they use, many nodes in the heap graph represent internal
 * implementation details of those APIs, with which the developer has no
 * reason to be familiar. For example, from the developer's point of view,
 * a JavaScript object is a collection of named properties with values.
 * However, at the C++ level, SpiderMonkey stores properties' values in
 * anonymous, numbered "slots" on the object, and then uses separate
 * "shape" and "base shape" objects to associate property names with slots.
 * Any analysis intended for use by web developers must present
 * object-to-value edges as named properties, as the developer expects, and
 * never mention slots and shapes. Similarly, Firefox represents DOM
 * elements as JavaScript objects pointing to C++ objects, with DOM parent
 * and child links stored in the latter; but analyses should present the
 * JavaScript object and its associated C++ object as a single entity.
 *
 * This header declares a C++ interface, UniformHeap, for traversing those
 * parts of the heap graph whose memory consumption is relevant to
 * JavaScript developers. A single type, UniformHeap::Node, can represent
 * any sort of object in the heap, whether it is a JavaScript object, an
 * XPCOM object, or some other type. Given a UniformHeap::Node, one can
 * enumerate the node's outgoing edges, find edge names, and get
 * UniformHeap::Node values for their referents. One can find information
 * about the node itself: its size in bytes, and perhaps identifying
 * information such as where it was was allocated. UniformHeap::Node values
 * compare equal when they refer to the same object; have hash values that
 * respect their equality relation; and can be serialized and deserialized
 * in a way that preserves their identity. UniformHeap::Node values are
 * small, and require no supporting data structures, making them feasible
 * for use in memory-constrained devices&mdash;ideally, the memory
 * requirements of the algorithm which uses them will be the limiting
 * factor, not the demands of UniformHeap::Node itself. Given this
 * interface, one can implement heap analysis algorithms independent of the
 * details of the heap under study. Furthermore, with appropriate changes
 * to UniformHeap, Firefox's heap itself should be able to evolve without
 * breaking existing analyses.
 *
 * While UniformHeap may omit non-owning or redundant edges, it generally
 * presents the heap as it actually exists in memory, and thus includes
 * nodes and edges corresponding to internal implementation details, which
 * may have no counterpart in the published specifications. To help
 * developer tools produce a view of the graph JavaScript developers would
 * find familiar, each UniformHeap::Node carries a <i>visible</i> flag,
 * indicating whether it represents an object that someone familiar with
 * public specifications would recognize, or an internal detail. Each graph
 * edge carries a similar flag. Given these annotations, a tool can present
 * a collapsed view of the graph that includes only visible nodes: starting
 * from some visible node <i>v</i>, the tool treats all visible nodes
 * reachable from <i>v</i> by a path containing only non-visible nodes as
 * if they were <i>v</i>'s immediate neighbors. The tool constructs names
 * for these artificial edges by composing the names of visible edges along
 * the path.
 *
 * (Why must UniformHeap expose non-visible nodes at all? Heap analyses
 * compute facts about the heap&mdash;for example, dominator analysis
 * produces the retained size of each node. However, these are facts about
 * the true graph; how one can best present them in the collapsed view,
 * which includes only visible nodes, depends on the analysis. Only the
 * analysis code itself can accurately and meaningfully aggregate its
 * results. Thus, UniformHeap exposes the information needed to do so, but
 * leaves the details to its clients.)
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

#define UNIFORMHEAP_NODE_KINDS(f)       \
    f(Object,    JSObject)              \
    f(String,    JSString)              \
    f(Script,    JSScript)              \
    f(Ioncode,   js::ion::IonCode)      \
    f(Shape,     js::Shape)             \
    f(BaseShape, js::BaseShape)         \
    /* end */

class UniformHeap {
  public:
    class Node {
        enum Kind {
#define UNIFORMHEAP_DECL(name, referentType) kind ## name,
            UNIFORMHEAP_NODE_KINDS(UNIFORMHEAP_DECL)
            kindCount
#undef UNIFORMHEAP_DECL
        };

#ifdef JS_BYTES_PER_WORD == 8
      private:
        intptr_t tagged;
        static const int tag_bits = 4;
        void *pointer() const { return reinterpret_cast<void *>(tagged >> tag_bits); }
        Kind tag() const { return reinterpret_cast<Kind>(tagged & ((1 << tag_bits) - 1)); }

      public:
        Node(void *pointer, Kind tag) {
            assert(0 <= tag && tag < kindCount);
            intptr_t address = reinterpret_cast<intptr_t>(pointer);
            tagged = (address << tag_bits) | reinterpret_cast<intptr_t>(tag);
            assert(this.pointer() == pointer);
            assert(this.tag() == tag);
        }
        Node (const Node &rhs) : tagged(rhs.tagged) { }
        Node &operator=(const Node &rhs) { tagged = rhs.tagged; }
        bool operator==(const Node &rhs) const { return tagged == rhs.tagged; }

        struct Hasher {
            typedef Node Lookup;
            static js::HashNumber hash(const Lookup &l) { return l.tagged; }
            static bool match(const Node &key, const Lookup &l) {
                return key == l;
            }
        };

#else
#error "mozilla::UniformHeap::Node not implemented for target architecture"
#endif
    };

};

#undef UNIFORMHEAP_NODE_KINDS

}  // namespace mozilla

#endif // mozilla_tools_HeapWalk_h
