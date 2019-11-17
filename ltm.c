/*
** $Id: ltm.c,v 2.38 2016/12/22 13:08:50 roberto Exp $
** Tag methods
** See Copyright Notice in lua.h
*/

#define ltm_c
#define LUA_CORE

#include "lprefix.h"


#include <string.h>

#include "lua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"
#include "ltm.h"
#include "lvm.h"


static const char udatatypename[] = "userdata";

LUAI_DDEF const char *const luaT_typenames_[LUA_TOTALTAGS] = {
  "no value",
  "nil", "boolean", udatatypename, "number",
  "string", "table", "function", udatatypename, "thread",
  "proto" /* this last case is used for tests only */
};


// 初始化元方法 在Lua虚拟机中注册元方法的名称并保证它们不会被回收
void luaT_init (lua_State *L) {
  static const char *const luaT_eventname[] = {  /* ORDER TM 所有元方法的名称 */
    "__index", "__newindex",
    "__gc", "__mode", "__len", "__eq",
    "__add", "__sub", "__mul", "__mod", "__pow",
    "__div", "__idiv",
    "__band", "__bor", "__bxor", "__shl", "__shr",
    "__unm", "__bnot", "__lt", "__le",
    "__concat", "__call"
  };
  int i;
  for (i=0; i<TM_N; i++) {
    G(L)->tmname[i] = luaS_new(L, luaT_eventname[i]);
    luaC_fix(L, obj2gco(G(L)->tmname[i]));  /* never collect these names 不要对这些值进行垃圾回收 */
  }
}


/*
** function to be used with macro "fasttm": optimized for absence of
** tag methods
*/
// 查询元方法的第一种方式
// 用于fasttm查询元方法 使用了 Table 结构中flags项进行缓存
const TValue *luaT_gettm (Table *events, TMS event, TString *ename) {
  const TValue *tm = luaH_getshortstr(events, ename);  // 从表中查询表项
  lua_assert(event <= TM_EQ);
  if (ttisnil(tm)) {  /* no tag method? 没有查询到元方法进行缓存 */
    events->flags |= cast_byte(1u<<event);  /* cache this fact 在flags项上设置该元方法位置为1 */
    return NULL;
  }
  else return tm;
}

// 查询元方法的第二种方式 可以查询任意对象中的任意元方法
// lua中的每个值都可以有一个元表 只是table和userdata可以有各自独立的元表 而其他类型的值则共享其类型所属的单一元表
// lua代码中只能设置table的元表 至于其他类型值的元表只能通过C代码设置
const TValue *luaT_gettmbyobj (lua_State *L, const TValue *o, TMS event) {
  Table *mt;
  switch (ttnov(o)) {   // 判断o的主类型
    case LUA_TTABLE:    // 获取Table类型的元表
      mt = hvalue(o)->metatable;
      break;
    case LUA_TUSERDATA: // 获取Userdata类型的元表
      mt = uvalue(o)->metatable;
      break;
    default:            // 其他基础类型的元表
      mt = G(L)->mt[ttnov(o)];
  }
  // 从元表中取出要查询的元方法 就是上一个函数的第一句 少了缓存的过程
  // 上一个函数是在元表中查询元方法
  // 这个函数是要先找出元表 再在元表中查询元方法
  return (mt ? luaH_getshortstr(mt, G(L)->tmname[event]) : luaO_nilobject);
}


/*
** Return the name of the type of an object. For tables and userdata
** with metatable, use their '__name' metafield, if present.
*/
// 返回一个TValue的类型名 如果table和userdata的元表中有__name键那么就使用这个键的值
// 这个函数并不是lua内置函数type调用的 只有在输出调试信息(ldebug.c文件)时会调用
/* lua 代码
   a = {}
   m = {__name = 'hello'}
   setmetatable(a, m)
   a < a
   会打印报错信息 attempt to compare two hello values
 */
const char *luaT_objtypename (lua_State *L, const TValue *o) {
  Table *mt;
  if ((ttistable(o) && (mt = hvalue(o)->metatable) != NULL) ||
      (ttisfulluserdata(o) && (mt = uvalue(o)->metatable) != NULL)) {
    const TValue *name = luaH_getshortstr(mt, luaS_new(L, "__name"));
    if (ttisstring(name))  /* is '__name' a string? */
      return getstr(tsvalue(name));  /* use it as type name */
  }
  return ttypename(ttnov(o));  /* else use standard type name */
}


void luaT_callTM (lua_State *L, const TValue *f, const TValue *p1,
                  const TValue *p2, TValue *p3, int hasres) {
  ptrdiff_t result = savestack(L, p3);
  StkId func = L->top;
  setobj2s(L, func, f);  /* push function (assume EXTRA_STACK) */
  setobj2s(L, func + 1, p1);  /* 1st argument */
  setobj2s(L, func + 2, p2);  /* 2nd argument */
  L->top += 3;
  if (!hasres)  /* no result? 'p3' is third argument */
    setobj2s(L, L->top++, p3);  /* 3rd argument */
  /* metamethod may yield only when called from Lua code */
  if (isLua(L->ci))
    luaD_call(L, func, hasres);
  else
    luaD_callnoyield(L, func, hasres);
  if (hasres) {  /* if has result, move it to its place */
    p3 = restorestack(L, result);
    setobjs2s(L, p3, --L->top);
  }
}


int luaT_callbinTM (lua_State *L, const TValue *p1, const TValue *p2,
                    StkId res, TMS event) {
  const TValue *tm = luaT_gettmbyobj(L, p1, event);  /* try first operand */
  if (ttisnil(tm))
    tm = luaT_gettmbyobj(L, p2, event);  /* try second operand */
  if (ttisnil(tm)) return 0;
  luaT_callTM(L, tm, p1, p2, res, 1);
  return 1;
}


void luaT_trybinTM (lua_State *L, const TValue *p1, const TValue *p2,
                    StkId res, TMS event) {
  if (!luaT_callbinTM(L, p1, p2, res, event)) {
    switch (event) {
      case TM_CONCAT:
        luaG_concaterror(L, p1, p2);
      /* call never returns, but to avoid warnings: *//* FALLTHROUGH */
      case TM_BAND: case TM_BOR: case TM_BXOR:
      case TM_SHL: case TM_SHR: case TM_BNOT: {
        lua_Number dummy;
        if (tonumber(p1, &dummy) && tonumber(p2, &dummy))
          luaG_tointerror(L, p1, p2);
        else
          luaG_opinterror(L, p1, p2, "perform bitwise operation on");
      }
      /* calls never return, but to avoid warnings: *//* FALLTHROUGH */
      default:
        luaG_opinterror(L, p1, p2, "perform arithmetic on");
    }
  }
}


int luaT_callorderTM (lua_State *L, const TValue *p1, const TValue *p2,
                      TMS event) {
  if (!luaT_callbinTM(L, p1, p2, L->top, event))
    return -1;  /* no metamethod */
  else
    return !l_isfalse(L->top);
}

