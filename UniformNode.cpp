#include "jsapi.h"
#include "jscntxt.h"
#include "vm/UniformNode.h"

namespace js {
namespace uniformnode {

#define DEF_KINDNAME(Name, Type) const char *Name ## Details::kindName = # Name;
UNIFORMNODE_FOR_EACH_KIND(DEF_KINDNAME)
#undef DEF_KINDNAME

Node::Node(void *thing, JSGCTraceKind kind)
{
    switch (kind) {
#     define INIT_CASE(Name, Type, TracerEnum)                                \
      case TracerEnum:                                                        \
        init(thing, kind ## Name);                                            \
        break;
      UNIFORMNODE_FOR_EACH_JSTRACER_KIND(INIT_CASE)
      default:
          MOZ_ASSUME_UNREACHABLE("Invalid JSGCTraceKind passed to Node constructor");
    }
}

// Generate a Node::match class to fetch a static member of the variant.
#define GET_STATIC(TYPE, MEMBER)                                              \
struct GetStatic_ ## MEMBER {                                                 \
    typedef TYPE Result;                                                      \
    template<typename T>                                                      \
    TYPE operator()(T *ptr) const { return Variant<T>::MEMBER; }              \
};

// Generate a Node::match class to call an accessor function on the variant
// instance.
#define CALL_ACCESSOR(TYPE, MEMBER)                                             \
struct CallGetter_ ## MEMBER {                                                  \
    typedef TYPE Result;                                                        \
    template<typename T>                                                        \
    Result operator()(T *ptr) const { return Variant<T>(ptr).MEMBER(); }        \
}

GET_STATIC(const char *, kindName);
const char *Node::kindName() const { return match(GetStatic_kindName()); }

CALL_ACCESSOR(bool, visible);
bool Node::visible() const { return match(CallGetter_visible()); }

CALL_ACCESSOR(JSCompartment *, compartment);
JSCompartment *Node::compartment() const { return match(CallGetter_compartment()); }

CALL_ACCESSOR(jschar *, constructor);
jschar *Node::constructor() const { return match(CallGetter_constructor()); }

GET_STATIC(bool, hasRefCount);
bool Node::hasRefCount() const { return match(GetStatic_hasRefCount()); }

// Node::match case class for dynamicEdgeRange. (Ideally, this class would
// be local to dynamicEdgeRange, but local classes can't have member
// templates.)
struct MakeDynamicEdgeRange {
  typedef DynamicEdgeRange *Result;
  JSContext *cx;
  MakeDynamicEdgeRange(JSContext *cx) : cx(cx) { }
  template<typename T>
  DynamicEdgeRange *operator()(T *ptr) const {
    typename Variant<T>::DynamicEdgeRange *r =
      new typename Variant<T>::DynamicEdgeRange();
    if (!r->init(cx, ptr)) {
      delete r;
      return NULL;
    }
    return r;
  }
};

DynamicEdgeRange *
Node::dynamicEdgeRange(JSContext *cx) const {
  return match(MakeDynamicEdgeRange(cx));
}

template<typename Edge>
class VectorBuildingTracer : public JSTracer {
    mozilla::Vector<Edge> *vec;

    static void staticCallback(JSTracer *trc, void **thingp, JSGCTraceKind kind) {
        static_cast<VectorBuildingTracer *>(trc)->callback(thingp, kind);
    }

    void callback(void **thingp, JSGCTraceKind kind) {
        if (!okay)
            return;

        // Ask the tracer to compute an edge name for us.
        char buffer[1024];
        char *name = strdup(JS_GetTraceEdgeName(this, buffer, sizeof(buffer)));
        if (!name) {
            okay = false;
            return;
        }

        if (!vec->growBy(1)) {
            free(name);
            okay = false;
            return;
        }

        vec->back() = mozilla::Move(SimpleEdge(name, Node(*thingp, kind)));
    }

  public:
    bool okay;
    VectorBuildingTracer(JSContext *cx, mozilla::Vector<Edge> *vec) : vec(vec), okay(true) {
        JS_TracerInit(this, JS_GetRuntime(cx), staticCallback);
    }
};

bool
RuntimeDetails::EdgeRange::init(JSContext *cx, const JSRuntime *rt)
{
    JS_ASSERT(cx->runtime() == rt);

    VectorBuildingTracer<Edge> tracer(cx, &edges);

    JS_TraceRuntime(&tracer);

    return tracer.okay;
}

bool
JSTracerEdgeRange::init(JSContext *cx, void *thing, JSGCTraceKind kind)
{
    VectorBuildingTracer<SimpleEdge> tracer(cx, &edges);

    JS_TraceChildren(&tracer, thing, kind);

    return tracer.okay;
}



// JSTracerEdgeRange

}  // namespace uniformnode
}  // namespace js
