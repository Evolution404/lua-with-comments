/*
** $Id: ltm.h,v 2.22 2016/02/26 19:20:15 roberto Exp $
** Tag methods
** See Copyright Notice in lua.h
*/

#ifndef ltm_h
#define ltm_h


#include "lobject.h"


/*
* WARNING: if you change the order of this enumeration,
* grep "ORDER TM" and "ORDER OP"
*/
// 所有的标签方法
typedef enum {
  TM_INDEX,    // 用于查询不存在的key
  TM_NEWINDEX, // 用于设置不存在的key 在给不存在的key赋值时会调用
  TM_GC,       // 对象被gc时调用
  TM_MODE,     // 弱表属性
  TM_LEN,      // #(取长度)操作
  TM_EQ,       // ==(等于)操作 last tag method with fast access 快速访问的最后一个元方法
  TM_ADD,      // +操作, 任何不是数字值(不包括能转换成数字的字符串)进行加法会尝试调用元方法
  TM_SUB,      // -操作, 与add类似
  TM_MUL,      // *操作
  TM_MOD,      // %操作
  TM_POW,      // ^(次方)操作
  TM_DIV,      // /操作
  TM_IDIV,     // //整除操作
  TM_BAND,     // &(按位与)操作
  TM_BOR,      // |(按位或)操作
  TM_BXOR,     // ~(按位异或)操作
  TM_SHL,      // <<(左移)操作
  TM_SHR,      // >>(右移)操作
  TM_UNM,      // -(取负)操作
  TM_BNOT,     // ~(按位非)操作
  TM_LT,       // <(小于)操作
  TM_LE,       // <=(小于等于)操作
  TM_CONCAT,   // ..(连接)操作
  TM_CALL,     // 函数调用操作
  TM_N		/* number of elements in the enum */
} TMS;

//查询元表的方式有两种：
// 方式一、查询某个元表（Table结构体）的元方法（见fasttm, gfasttm）
//         使用了缓存可以快速查询 但是只能查询TM_EQ以及它之前的元方法
// 方式二、查询某个对象的元方法（见luaT_gettmbyobj）
//         可以查询所有的元方法

// 查询某个元表（Table结构体）的元方法
// et是要查询的元表 e是要查询的事件(元方法的枚举值)
// 如果et是NULL或者flags中缓存结果是NULL那么直接返回NULL
// 否则使用luaT_gettm查询元方法
#define gfasttm(g,et,e) ((et) == NULL ? NULL : \
  ((et)->flags & (1u<<(e))) ? NULL : luaT_gettm(et, e, (g)->tmname[e]))

// 从et中查询元方法e 查询不到返回NULL
#define fasttm(l,et,e)	gfasttm(G(l), et, e)

#define ttypename(x)	luaT_typenames_[(x) + 1]

LUAI_DDEC const char *const luaT_typenames_[LUA_TOTALTAGS]; // 各种数据类型的名称


LUAI_FUNC const char *luaT_objtypename (lua_State *L, const TValue *o);

LUAI_FUNC const TValue *luaT_gettm (Table *events, TMS event, TString *ename);
LUAI_FUNC const TValue *luaT_gettmbyobj (lua_State *L, const TValue *o,
                                                       TMS event);
LUAI_FUNC void luaT_init (lua_State *L);

LUAI_FUNC void luaT_callTM (lua_State *L, const TValue *f, const TValue *p1,
                            const TValue *p2, TValue *p3, int hasres);
LUAI_FUNC int luaT_callbinTM (lua_State *L, const TValue *p1, const TValue *p2,
                              StkId res, TMS event);
LUAI_FUNC void luaT_trybinTM (lua_State *L, const TValue *p1, const TValue *p2,
                              StkId res, TMS event);
LUAI_FUNC int luaT_callorderTM (lua_State *L, const TValue *p1,
                                const TValue *p2, TMS event);



#endif
