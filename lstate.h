/*
** $Id: lstate.h,v 2.133 2016/12/22 13:08:50 roberto Exp $
** Global State
** See Copyright Notice in lua.h
*/

#ifndef lstate_h
#define lstate_h

#include "lua.h"

#include "lobject.h"
#include "ltm.h"
#include "lzio.h"


/*

** Some notes about garbage-collected objects: All objects in Lua must
** be kept somehow accessible until being freed, so all objects always
** belong to one (and only one) of these lists, using field 'next' of
** the 'CommonHeader' for the link:
**
** 'allgc': all objects not marked for finalization;
** 'finobj': all objects marked for finalization;
** 'tobefnz': all objects ready to be finalized;
** 'fixedgc': all objects that are not to be collected (currently
** only small strings, such as reserved words).

*/


struct lua_longjmp;  /* defined in ldo.c */


/*
** Atomic type (relative to signals) to better ensure that 'lua_sethook'
** is thread safe
*/
#if !defined(l_signalT)
#include <signal.h>
#define l_signalT	sig_atomic_t
#endif


/* extra stack space to handle TM calls and some other extras */
#define EXTRA_STACK   5


#define BASIC_STACK_SIZE        (2*LUA_MINSTACK)


/* kinds of Garbage Collection */
#define KGC_NORMAL	0
#define KGC_EMERGENCY	1	/* gc was forced by an allocation failure */


typedef struct stringtable {
  TString **hash;
  int nuse;  /* number of elements */
  int size;
} stringtable;


/*
** Information about a call.
** When a thread yields, 'func' is adjusted to pretend that the
** top function has only the yielded values in its stack; in that
** case, the actual 'func' value is saved in field 'extra'.
** When a function calls another with a continuation, 'extra' keeps
** the function index so that, in case of errors, the continuation
** function can be called with the correct top.
*/
typedef struct CallInfo {
  StkId func;  /* function index in the stack */
  StkId	top;  /* top for this function */
  struct CallInfo *previous, *next;  /* dynamic call link */
  union {
    struct {  /* only for Lua functions */
      StkId base;  /* base for this function */
      const Instruction *savedpc;
    } l;
    struct {  /* only for C functions */
      lua_KFunction k;  /* continuation in case of yields */
      ptrdiff_t old_errfunc;
      lua_KContext ctx;  /* context info. in case of yields */
    } c;
  } u;
  ptrdiff_t extra;
  short nresults;  /* expected number of results from this function */
  unsigned short callstatus;
} CallInfo;


/*
** Bits in CallInfo status
*/
// CIST:CallInfo status
#define CIST_OAH	(1<<0)	/* original value of 'allowhook' */
#define CIST_LUA	(1<<1)	/* call is running a Lua function */
#define CIST_HOOKED	(1<<2)	/* call is running a debug hook */
#define CIST_FRESH	(1<<3)	/* call is running on a fresh invocation
                                   of luaV_execute */
#define CIST_YPCALL	(1<<4)	/* call is a yieldable protected call */
#define CIST_TAIL	(1<<5)	/* call was tail called */
#define CIST_HOOKYIELD	(1<<6)	/* last hook called yielded */
#define CIST_LEQ	(1<<7)  /* using __lt for __le */
#define CIST_FIN	(1<<8)  /* call is running a finalizer */

#define isLua(ci)	((ci)->callstatus & CIST_LUA)

/* assume that CIST_OAH has offset 0 and that 'v' is strictly 0/1 */
#define setoah(st,v)	((st) = ((st) & ~CIST_OAH) | (v))
#define getoah(st)	((st) & CIST_OAH)


/*
** 'global state', shared by all threads of this state
*/

/*
  // global_State分模块解释
  // Version
  const lua_Number *version;

  // Hash
  unsigned int seed;                         // 哈希表使用的随机数种子

  // Registry
  // 是一个长度为2的数组类型表
  // 分别放了主线程和全局注册表,全局注册表其实就是_ENV变量
  TValue l_registry;

  // String table
  stringtable strt;                          // 全局字符串表 存储所有短字符串
  TString *strcache[STRCACHE_N][STRCACHE_M]; // 字符串缓存 新建字符串全部存入strcache中 全部初始化为MEMERRMSG

  // Meta table
  TString *tmname[TM_N];                     // 在luaT_init中初始化 设置为所有元方法的名称的数组
  struct Table *mt[LUA_NUMTAGS];             // 存储基本类型的元表

  // Thread list
  struct lua_State *mainthread;              // 主线程
  struct lua_State *twups;                   // thread with upvalues 带有upvalues的线程的数组

  // Error Recover
  lua_CFunction panic;                       // 全局错误处理响应点

  // Memory Allocator
  lua_Alloc frealloc;                        // lua使用的内存分配的函数 可以用户自定义
  void *ud;                                  // 内存分配器的userdata

  // GC
  lu_mem totalbytes;
  TString *memerrmsg;

  // 没有被回收器补偿的分配字节,预估内存借债,新申请的内存超过此值,则触发GC 这个值可正可负
  // luaM_realloc_,luaE_setdebt中修改了GCdebt
  // 当新增分配内存时,GCdebt值将会增加,即GC需要释放的内存增加;当释放内存时,GCdebt将会减少,即GC需要释放的内存减少
  // GCdebt大于零则意味着有需要GC释放还未释放的内存,所以会触发GC
  l_mem  GCdebt;
  // 单步中GC遍历的内存记录
  lu_mem GCmemtrav;
  // 使用中的非垃圾内存的估值(与当前内存实际分配数量大约相等)
  lu_mem GCestimate;

  // 当前使用的是white0还是white1
  lu_byte currentwhite;
  lu_byte gcstate;
  lu_byte gckind;
  lu_byte gcrunning;

  int gcpause;
  int gcstepmul;
  unsigned int gcfinnum;

  // 在任意时刻,所有gc对象都存在于以下四个链表之一
  // 每新建一个gc对象,都将这个对象串联到allgc链表的头上
  GCObject *allgc;   // 所有未被标记为终结的对象
  // 如果对象元表有__gc元方法,那么它会从allgc取出,加入到finobj去
  // luaC_checkfinalizer函数完成了从allgc到finobj的移动
  // 并设置对象的FINALIZEDBIT标记位
  GCObject *finobj;  // 所有被标记为终结的对象
  // 在标记阶段,finobj中的白对象会移到tobefnz链表去,然后标记这些对象,这样当前周期不会释放这些对象
  // 清扫完之后会进入GCScallfin阶段,在这里调用tobefnz对象的gc方法,同时把对象移回allgc链表
  // 如果gc中使对象重新变成可到达，则对象复活过来；否则下个周期这个对象就会被正常清除。
  GCObject *tobefnz; // 所有准备终结的对象(准备调用__gc的对象)
  // fixedgc是那些不会被回收的对象
  // 在新建完对象后,必须马上调用luaC_fix把对象从allgc移到fixedgc去
  // GC的过程不会对fixedgc进行清扫
  GCObject *fixedgc; // 不会被回收的对象

  GCObject *gray;
  // grayagain是需要原子操作标记的灰色节点
  // 由于线程对象的引用关系变化非常频繁,就是说栈中元素一直在变化
  // 所以就强制线程对象在原子阶段重新扫描一遍
  GCObject *grayagain;

  // 以下三个链表只在原子阶段有效
  // 保存所有的弱值表
  GCObject *weak;
  // 保存弱键表
  GCObject *ephemeron;
  // 保存全弱表
  GCObject *allweak;

  GCObject **sweepgc;
*/

typedef struct global_State {
  lua_Alloc frealloc;  /* function to reallocate memory */
  void *ud;         /* auxiliary data to 'frealloc' */
  l_mem totalbytes;  /* number of bytes currently allocated - GCdebt */
  l_mem GCdebt;  /* bytes allocated not yet compensated by the collector */
  lu_mem GCmemtrav;  /* memory traversed by the GC */
  lu_mem GCestimate;  /* an estimate of the non-garbage memory in use */
  stringtable strt;  /* hash table for strings */
  TValue l_registry;
  unsigned int seed;  /* randomized seed for hashes */
  lu_byte currentwhite;
  lu_byte gcstate;  /* state of garbage collector */
  lu_byte gckind;  /* kind of GC running */
  lu_byte gcrunning;  /* true if GC is running */
  GCObject *allgc;  /* list of all collectable objects */
  GCObject **sweepgc;  /* current position of sweep in list */
  GCObject *finobj;  /* list of collectable objects with finalizers */
  GCObject *gray;  /* list of gray objects */
  GCObject *grayagain;  /* list of objects to be traversed atomically */
  GCObject *weak;  /* list of tables with weak values */
  GCObject *ephemeron;  /* list of ephemeron tables (weak keys) */
  GCObject *allweak;  /* list of all-weak tables */
  GCObject *tobefnz;  /* list of userdata to be GC */
  GCObject *fixedgc;  /* list of objects not to be collected */
  struct lua_State *twups;  /* list of threads with open upvalues */
  unsigned int gcfinnum;  /* number of finalizers to call in each GC step */
  int gcpause;  /* size of pause between successive GCs */
  int gcstepmul;  /* GC 'granularity' */
  lua_CFunction panic;  /* to be called in unprotected errors */
  struct lua_State *mainthread;
  const lua_Number *version;  /* pointer to version number */
  TString *memerrmsg;  /* memory-error message */
  TString *tmname[TM_N];  /* array with tag-method names */
  struct Table *mt[LUA_NUMTAGS];  /* metatables for basic types */
  TString *strcache[STRCACHE_N][STRCACHE_M];  /* cache for strings in API */
} global_State;


/*
** 'per thread' state
*/

/*
// lua_State为了内存对齐进行了重新排布
struct lua_State {
  CommonHeader;

  // 1 
  // l_G是Lua的全局对象 所有lua_State共用一个global_State 其中包含了很多全局字段
  global_State *l_G;

  // 2
  // 当前thread的状态 LUA_OK 跳转查看
  lu_byte status;

  // 3 
  // thread的运行过程就是指令不断执行的过程 oldpc就是指向最后执行的指令
  const Instruction *oldpc;  

  // 4 Data Stack(数据栈): [stack,...,top,...,stack_last], length is stacksize
  // 数据栈相关的信息 StkId就是TValue 用于语义上的区分 数据栈实际上就是TValue的数组
  // stack是栈底 top是当前栈顶 stack_last是为栈已经分配空间的最后位置
  // top到stack_last是剩余空间
  int stacksize;           // stacksize 包括EXTRA_STACK
  StkId top;                 
  StkId stack;               
  StkId stack_last;        // stack_last 指向EXTRA_STACK前的最后一个位置
  
  // 5 Call Stack(调用栈)
  // 与数据栈不同 调用栈是CallInfo构成的链表
  CallInfo base_ci;        base_ci是调用栈的栈底
  CallInfo *ci;            调用栈的栈顶 当前执行的函数  
  
  unsigned short nCcalls;  嵌套的C函数的个数
  unsigned short nny;      调用栈中non-yieldable的调用个数

  // 6 Up Value
  UpVal *openupval;         将栈中所有开放的upvalue串成链表
  struct lua_State *twups;   

  // 7 Recover
  // errorJmp是一个lua_longjmp对象组成的链表,在luaD_rawrunprotected中链接
  struct lua_longjmp *errorJmp;  
  ptrdiff_t errfunc;  

  // 8 Hook for Debug
  lua_Hook hook;
  int basehookcount;
  int hookcount;
  lu_byte hookmask;
  lu_byte allowhook;

  // 9
  GCObject *gclist;
}; 
 */
struct lua_State {
  CommonHeader;
  unsigned short nci;  /* number of items in 'ci' list */
  lu_byte status;
  StkId top;  /* first free slot in the stack */
  global_State *l_G;
  CallInfo *ci;  /* call info for current function */
  const Instruction *oldpc;  /* last pc traced */
  StkId stack_last;  /* last free slot in the stack */
  StkId stack;  /* stack base */
  UpVal *openupval;  /* list of open upvalues in this stack */
  GCObject *gclist;
  struct lua_State *twups;  /* list of threads with open upvalues */
  struct lua_longjmp *errorJmp;  /* current error recover point */
  CallInfo base_ci;  /* CallInfo for first level (C calling Lua) */
  volatile lua_Hook hook;
  // 在docall函数中使用msghandler作为了错误处理函数
  ptrdiff_t errfunc;  /* current error handling function (stack index) */
  int stacksize;
  int basehookcount;
  int hookcount;
  unsigned short nny;  /* number of non-yieldable calls in stack */
  unsigned short nCcalls;  /* number of nested C calls */
  l_signalT hookmask;
  lu_byte allowhook;
};


#define G(L)	(L->l_G)


/*
** Union of all collectable objects (only for conversions)
*/
// 一个所有需要回收的类型的共用体,只用于类型转换
// GCUnion中各个项除了Closure都是以common header开始的
// 所以在强制类型转换的时候引用common header里的项不会有问题
// 例如TString以common header开头, 强制转换为GCUnion访问gc项与直接ts->gc是一样的
// 所有垃圾回收类型在创建时都调用了luaC_newobj来完成对GCObject的初始化
union GCUnion {
  GCObject gc;  /* common header */
  // createstrobj中调用luaC_newobj
  struct TString ts;
  // luaS_newudata中调用luaC_newobj
  struct Udata u;
  // luaF_newCclosure,luaF_newLclosure中都调用了luaC_newobj
  union Closure cl;
  // luaH_new中调用了luaC_newobj
  struct Table h;
  // luaF_newproto中调用了luaC_newobj
  struct Proto p;
  // lua_newstate中没调用luaC_newobj,但是对marked字段进行了处理
  struct lua_State th;  /* thread */
};


#define cast_u(o)	cast(union GCUnion *, (o))

/* macros to convert a GCObject into a specific value */
// 用于将GCObject转换成各种数据类型
// 这里使用cast_u的目的可能是简化写法
// 例如gco2ts这里使用 &((cast_u(o))->ts) 换成 cast(TString*, o) 也是完全可以的
#define gco2ts(o)  \
	check_exp(novariant((o)->tt) == LUA_TSTRING, &((cast_u(o))->ts))
#define gco2u(o)  check_exp((o)->tt == LUA_TUSERDATA, &((cast_u(o))->u))
#define gco2lcl(o)  check_exp((o)->tt == LUA_TLCL, &((cast_u(o))->cl.l))
#define gco2ccl(o)  check_exp((o)->tt == LUA_TCCL, &((cast_u(o))->cl.c))
#define gco2cl(o)  \
	check_exp(novariant((o)->tt) == LUA_TFUNCTION, &((cast_u(o))->cl))
#define gco2t(o)  check_exp((o)->tt == LUA_TTABLE, &((cast_u(o))->h))
#define gco2p(o)  check_exp((o)->tt == LUA_TPROTO, &((cast_u(o))->p))
#define gco2th(o)  check_exp((o)->tt == LUA_TTHREAD, &((cast_u(o))->th))


/* macro to convert a Lua object into a GCObject */
// 将可回收类型指针 转换成 GCObject*类型
// 先转换成gcunion再取出gcunion的gc字段
// 经过测试将 &(cast_u(v)->gc) 换成 cast(GCObject *, (v))效果是一样的
// 实质上就是将v指针的类型转换成GCObject* 不经过GCUnion进行中转也是一样的
// 其实改成 cast_u(v)编译也不会报错 只是会报出一些类型不匹配的警告
#define obj2gco(v) \
  check_exp(novariant((v)->tt) < LUA_TDEADKEY, (&(cast_u(v)->gc)))
	//check_exp(novariant((v)->tt) < LUA_TDEADKEY, (cast(GCObject *, (v))))


/* actual number of total bytes allocated */
// 实际被分配的所有内存空间
#define gettotalbytes(g)	cast(lu_mem, (g)->totalbytes + (g)->GCdebt)

LUAI_FUNC void luaE_setdebt (global_State *g, l_mem debt);
LUAI_FUNC void luaE_freethread (lua_State *L, lua_State *L1);
LUAI_FUNC CallInfo *luaE_extendCI (lua_State *L);
LUAI_FUNC void luaE_freeCI (lua_State *L);
LUAI_FUNC void luaE_shrinkCI (lua_State *L);


#endif

