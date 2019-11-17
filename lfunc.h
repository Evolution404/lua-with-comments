/*
** $Id: lfunc.h,v 2.15 2015/01/13 15:49:11 roberto Exp $
** Auxiliary functions to manipulate prototypes and closures
** See Copyright Notice in lua.h
*/

#ifndef lfunc_h
#define lfunc_h


#include "lobject.h"


// 计算Cclosure的大小
// CClosure定义最后一项 TValue upvalue[1] 所有的upvalue都放在最后面
// 所以计算大小就是CClosure+(n-1)个TValue的大小(CClosure中已经有了一个)
#define sizeCclosure(n)	(cast(int, sizeof(CClosure)) + \
                         cast(int, sizeof(TValue)*((n)-1)))

// 计算Lclosure的大小
// 与上面不同的是LClosure最后面存的是指针不是实际的Upvalue
// 所以是TValue的指针大小*(n-1)
#define sizeLclosure(n)	(cast(int, sizeof(LClosure)) + \
                         cast(int, sizeof(TValue *)*((n)-1)))


/* test whether thread is in 'twups' list */
// L与twups不直接相等说明L在twups后面的链上
#define isintwups(L)	(L->twups != L)


/*
** maximum number of upvalues in a closure (both C and Lua). (Value
** must fit in a VM register.)
*/
// 一个闭包的upvalue上限
#define MAXUPVAL	255


/*
** Upvalues for Lua closures
*/
// Lua函数闭包变量
// 闭包变量有open和closed状态
// open状态是外层函数还没有返回 open字段用作当前作用域的闭包变量链表
// closed状态是外层函数返回将闭包变量的值拷贝出来 放在value字段
struct UpVal {
  TValue *v;  /* points to stack or to its own value 指向变量的实际值 */
  lu_mem refcount;  /* reference counter 引用计数 */
  union {
    struct {  /* (when open) */
      UpVal *next;  /* linked list */
      int touched;  /* mark to avoid cycles with dead threads */
    } open;
    TValue value;  /* the value (when closed) */
  } u;
};

// 当v字段和value字段相等说明进入了closed状态
#define upisopen(up)	((up)->v != &(up)->u.value)


LUAI_FUNC Proto *luaF_newproto (lua_State *L);
LUAI_FUNC CClosure *luaF_newCclosure (lua_State *L, int nelems);
LUAI_FUNC LClosure *luaF_newLclosure (lua_State *L, int nelems);
LUAI_FUNC void luaF_initupvals (lua_State *L, LClosure *cl);
LUAI_FUNC UpVal *luaF_findupval (lua_State *L, StkId level);
LUAI_FUNC void luaF_close (lua_State *L, StkId level);
LUAI_FUNC void luaF_freeproto (lua_State *L, Proto *f);
LUAI_FUNC const char *luaF_getlocalname (const Proto *func, int local_number,
                                         int pc);


#endif
