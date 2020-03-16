/*
** $Id: lgc.h,v 2.91 2015/12/21 13:02:14 roberto Exp $
** Garbage Collector
** See Copyright Notice in lua.h
*/

#ifndef lgc_h
#define lgc_h


#include "lobject.h"
#include "lstate.h"

/*
** Collectable objects may have one of three colors: white, which
** means the object is not marked; gray, which means the
** object is marked, but its references may be not marked; and
** black, which means that the object and all its references are marked.
** The main invariant of the garbage collector, while marking objects,
** is that a black object can never point to a white one. Moreover,
** any gray object must be in a "gray list" (gray, grayagain, weak,
** allweak, ephemeron) so that it can be visited again before finishing
** the collection cycle. These lists have no meaning when the invariant
** is not being enforced (e.g., sweep phase).
*/



/* how much to allocate before next GC step */
#if !defined(GCSTEPSIZE)
/* ~100 small strings */
// GCSTEPSIZE默认是1600
#define GCSTEPSIZE	(cast_int(100 * sizeof(TString)))
#endif


/*
** Possible states of the Garbage Collector
*/
#define GCSpropagate	0
#define GCSatomic	1
#define GCSswpallgc	2
#define GCSswpfinobj	3
#define GCSswptobefnz	4
#define GCSswpend	5
#define GCScallfin	6
#define GCSpause	7


// 在清扫阶段
#define issweepphase(g)  \
	(GCSswpallgc <= (g)->gcstate && (g)->gcstate <= GCSswpend)


/*
** macro to tell when main invariant (white objects cannot point to black
** ones) must be kept. During a collection, the sweep
** phase may break the invariant, as objects turned white may point to
** still-black objects. The invariant is restored when sweep ends and
** all objects are white again.
*/

// 返回非0代表当前标记阶段还没有完成
// 所谓invariant就是指黑对象不能指向白对象
// invariant在传播阶段和原子阶段有效,在清扫阶段不保证有效
// 所以原子阶段前keepinvariant(保持invariant)返回true
#define keepinvariant(g)	((g)->gcstate <= GCSatomic)


/*
** some useful bit tricks
*/
// m的哪一位标记为1,就设置x的哪一位为0
#define resetbits(x,m)		((x) &= cast(lu_byte, ~(m)))
// m的哪一位标记为1,就设置x的哪一位为1
#define setbits(x,m)		((x) |= (m))
// 存在m为1的位,x也为1 则返回1,否则为0
#define testbits(x,m)		((x) & (m))
#define bitmask(b)		(1<<(b))
#define bit2mask(b1,b2)		(bitmask(b1) | bitmask(b2))
#define l_setbit(x,b)		setbits(x, bitmask(b))
#define resetbit(x,b)		resetbits(x, bitmask(b))
#define testbit(x,b)		testbits(x, bitmask(b))


/* Layout for bit use in 'marked' field: */
#define WHITE0BIT	0  /* object is white (type 0) */
#define WHITE1BIT	1  /* object is white (type 1) */
#define BLACKBIT	2  /* object is black */
#define FINALIZEDBIT	3  /* object has been marked for finalization */
/* bit 7 is currently used by tests (luaL_checkmemory) */

// 同时标记white0和white1
#define WHITEBITS	bit2mask(WHITE0BIT, WHITE1BIT)


// 如果是white就返回true white0和white1都可
#define iswhite(x)      testbits((x)->marked, WHITEBITS)
// 判断是不是balck
#define isblack(x)      testbit((x)->marked, BLACKBIT)
// white0,white1,black都没有被标记,所以就是灰
#define isgray(x)  /* neither white nor black */  \
	(!testbits((x)->marked, WHITEBITS | bitmask(BLACKBIT)))

// 保留FINALIZEDBIT的结果,其他位都设置为0
#define tofinalize(x)	testbit((x)->marked, FINALIZEDBIT)

// currentwhite是white0就返回white1,currentwhite是white1就返回white0
#define otherwhite(g)	((g)->currentwhite ^ WHITEBITS)
// ow-otherwhite m-marked
// m是其他白说明对象已经死亡
#define isdeadm(ow,m)	(!(((m) ^ WHITEBITS) & (ow)))
// 判断一个对象是不是死亡了,也就是标记是其他白的对象
#define isdead(g,v)	isdeadm(otherwhite(g), (v)->marked)

// white0变为white1,或者white1变成white0
#define changewhite(x)	((x)->marked ^= WHITEBITS)
// 标记black位为1
#define gray2black(x)	l_setbit((x)->marked, BLACKBIT)

// 根据g->currentwhite返回当前是white0还是white1
#define luaC_white(g)	cast(lu_byte, (g)->currentwhite & WHITEBITS)


/*
** Does one step of collection when debt becomes positive. 'pre'/'pos'
** allows some adjustments to be done only when needed. macro
** 'condchangemem' is used only for heavy tests (forcing a full
** GC cycle on every opportunity)
*/
#define luaC_condGC(L,pre,pos) \
	{ if (G(L)->GCdebt > 0) { pre; luaC_step(L); pos;}; \
	  condchangemem(L,pre,pos); }

/* more often than not, 'pre'/'pos' are empty */
#define luaC_checkGC(L)		luaC_condGC(L,(void)0,(void)0)


// barrier中p是父对象,v是子对象 不论是向前向后,都要保证p是黑对象,v是白对象
// p是各种具体类型例如TString,Proto v是TValue类型
// 两种形式的barrier是出于性能的考虑
// 只有三种情况下会调用luaC_barrier,分别是:为C闭包设置上值,为full userdata设置用户数据,设置元表(metatable)
// 这些操作都有一个明显的特别,做这些操作时,只会改变少量的引用关系
// 这种情况下直接将父节点变灰是不值得的(因为变灰后,会重新遍历所有引用的值).

// barrier是保持父对象为黑,将子对象变灰
#define luaC_barrier(L,p,v) (  \
	(iscollectable(v) && isblack(p) && iswhite(gcvalue(v))) ?  \
	luaC_barrier_(L,obj2gco(p),gcvalue(v)) : cast_void(0))

// barrierback是将父对象变灰,子对象依旧为白
#define luaC_barrierback(L,p,v) (  \
	(iscollectable(v) && isblack(p) && iswhite(gcvalue(v))) ? \
	luaC_barrierback_(L,p) : cast_void(0))

#define luaC_objbarrier(L,p,o) (  \
	(isblack(p) && iswhite(o)) ? \
	luaC_barrier_(L,obj2gco(p),obj2gco(o)) : cast_void(0))

#define luaC_upvalbarrier(L,uv) ( \
	(iscollectable((uv)->v) && !upisopen(uv)) ? \
         luaC_upvalbarrier_(L,uv) : cast_void(0))

LUAI_FUNC void luaC_fix (lua_State *L, GCObject *o);
LUAI_FUNC void luaC_freeallobjects (lua_State *L);
LUAI_FUNC void luaC_step (lua_State *L);
LUAI_FUNC void luaC_runtilstate (lua_State *L, int statesmask);
LUAI_FUNC void luaC_fullgc (lua_State *L, int isemergency);
LUAI_FUNC GCObject *luaC_newobj (lua_State *L, int tt, size_t sz);
LUAI_FUNC void luaC_barrier_ (lua_State *L, GCObject *o, GCObject *v);
LUAI_FUNC void luaC_barrierback_ (lua_State *L, Table *o);
LUAI_FUNC void luaC_upvalbarrier_ (lua_State *L, UpVal *uv);
LUAI_FUNC void luaC_checkfinalizer (lua_State *L, GCObject *o, Table *mt);
LUAI_FUNC void luaC_upvdeccount (lua_State *L, UpVal *uv);


#endif
