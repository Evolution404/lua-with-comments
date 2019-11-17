/*
** $Id: lfunc.c,v 2.45 2014/11/02 19:19:04 roberto Exp $
** Auxiliary functions to manipulate prototypes and closures
** See Copyright Notice in lua.h
*/

#define lfunc_c
#define LUA_CORE

#include "lprefix.h"


#include <stddef.h>

#include "lua.h"

#include "lfunc.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"



// 传入CClosure的upvalue个数 分配内存,创建CClosure对象,设置nupvalues
CClosure *luaF_newCclosure (lua_State *L, int n) {
  GCObject *o = luaC_newobj(L, LUA_TCCL, sizeCclosure(n));
  CClosure *c = gco2ccl(o);
  c->nupvalues = cast_byte(n);
  return c;
}


// 传入LClosure的upvalue个数 分配内存,创建LClosure对象,设置nupvalues
// 比上一个函数多了初始化的工作
// 可能的原因是上一个函数不需要初始化 少了这一步提高效率
LClosure *luaF_newLclosure (lua_State *L, int n) {
  GCObject *o = luaC_newobj(L, LUA_TLCL, sizeLclosure(n));
  LClosure *c = gco2lcl(o);
  c->p = NULL;
  c->nupvalues = cast_byte(n);
  while (n--) c->upvals[n] = NULL;
  return c;
}

/*
** fill a closure with new closed upvalues
*/
// 为LClosure的upvalue设置closed状态的初始值
void luaF_initupvals (lua_State *L, LClosure *cl) {
  int i;
  for (i = 0; i < cl->nupvalues; i++) {
    UpVal *uv = luaM_new(L, UpVal);
    uv->refcount = 1;
    uv->v = &uv->u.value;  /* make it closed 当uv->v指向内部共用体的value时说明是closed状态 */
    setnilvalue(uv->v);    // 设置nil值只需要修改一下类型标记即可
    cl->upvals[i] = uv;
  }
}


// 在L_>openupval中查询upval
// 如果查询不到在l_>openupval链表头新建upval为要查询的值
UpVal *luaF_findupval (lua_State *L, StkId level) {
  UpVal **pp = &L->openupval; // 后面有可能修改L->openupval所以这里使用了二级指针
  UpVal *p;
  UpVal *uv;
  lua_assert(isintwups(L) || L->openupval == NULL);
  // 查找L->openupval链表 如果查询到直接返回
  while (*pp != NULL && (p = *pp)->v >= level) {
    lua_assert(upisopen(p));
    if (p->v == level)  /* found a corresponding upvalue? 判断是否是查询的upval */
      return p;  /* return it 查询到结果直接返回 */
    pp = &p->u.open.next;
  }
  /* not found: create a new upvalue 查询不到创建新的upval */
  uv = luaM_new(L, UpVal);
  uv->refcount = 0;
  uv->u.open.next = *pp;  /* link it to list of open upvalues 将新创建的upval放到L->openupval链表最开始 */
  uv->u.open.touched = 1;
  *pp = uv;
  uv->v = level;  /* current value lives in the stack 新创建的upval存放在数据栈中 还没有拷贝到UpVal中(open状态) */
  // 如果当前线程不在twups链表中 将当前线程加入链表中
  if (!isintwups(L)) {  /* thread not in list of threads with upvalues? */
    L->twups = G(L)->twups;  /* link it to the list */
    G(L)->twups = L;
  }
  return uv;
}


// 将level之上的所有upval进入closed状态(关闭)
// 如果一个upval没有引用 释放该upval
void luaF_close (lua_State *L, StkId level) {
  UpVal *uv;
  while (L->openupval != NULL && (uv = L->openupval)->v >= level) {
    lua_assert(upisopen(uv));
    L->openupval = uv->u.open.next;  /* remove from 'open' list */
    if (uv->refcount == 0)  /* no references? */
      luaM_free(L, uv);  /* free upvalue */
    else { // 设置upval进入closed状态
      setobj(L, &uv->u.value, uv->v);  /* move value to upvalue slot 从栈上拷贝值 */
      uv->v = &uv->u.value;  /* now current value lives here 修改指向值的指针 */
      luaC_upvalbarrier(L, uv);
    }
  }
}


// 创建proto对象 并初始化
Proto *luaF_newproto (lua_State *L) {
  GCObject *o = luaC_newobj(L, LUA_TPROTO, sizeof(Proto));
  Proto *f = gco2p(o);
  f->k = NULL;
  f->sizek = 0;
  f->p = NULL;
  f->sizep = 0;
  f->code = NULL;
  f->cache = NULL;
  f->sizecode = 0;
  f->lineinfo = NULL;
  f->sizelineinfo = 0;
  f->upvalues = NULL;
  f->sizeupvalues = 0;
  f->numparams = 0;
  f->is_vararg = 0;
  f->maxstacksize = 0;
  f->locvars = NULL;
  f->sizelocvars = 0;
  f->linedefined = 0;
  f->lastlinedefined = 0;
  f->source = NULL;
  return f;
}


// 释放一个proto对象
void luaF_freeproto (lua_State *L, Proto *f) {
  luaM_freearray(L, f->code, f->sizecode);
  luaM_freearray(L, f->p, f->sizep);
  luaM_freearray(L, f->k, f->sizek);
  luaM_freearray(L, f->lineinfo, f->sizelineinfo);
  luaM_freearray(L, f->locvars, f->sizelocvars);
  luaM_freearray(L, f->upvalues, f->sizeupvalues);
  luaM_free(L, f);
}


/*
** Look for n-th local variable at line 'line' in function 'func'.
** Returns NULL if not found.
*/
// 查询函数f中在pc行 第local_number个局部变量的名称
// 查询不到返回NULL
const char *luaF_getlocalname (const Proto *f, int local_number, int pc) {
  int i;
  for (i = 0; i<f->sizelocvars && f->locvars[i].startpc <= pc; i++) {
    if (pc < f->locvars[i].endpc) {  /* is variable active? */
      local_number--;
      if (local_number == 0)
        return getstr(f->locvars[i].varname);
    }
  }
  return NULL;  /* not found */
}

