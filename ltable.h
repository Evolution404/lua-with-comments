/*
** $Id: ltable.h,v 2.23 2016/12/22 13:08:50 roberto Exp $
** Lua tables (hash) Lua表的实现
** See Copyright Notice in lua.h
*/

#ifndef ltable_h
#define ltable_h

#include "lobject.h"


#define gnode(t,i)	(&(t)->node[i])
#define gval(n)		(&(n)->i_val)
#define gnext(n)	((n)->i_key.nk.next)


/* 'const' to avoid wrong writings that can mess up field 'next' */
// 从Node对象种查询出key(TValue对象)
#define gkey(n)		cast(const TValue*, (&(n)->i_key.tvk))

/*
** writable version of 'gkey'; allows updates to individual fields,
** but not to the whole (which has incompatible type)
*/
// 可以对key的各个域单独进行写操作
#define wgkey(n)		(&(n)->i_key.nk)

// 使元方法查询的缓存无效
// 其实就是让flags=0
#define invalidateTMcache(t)	((t)->flags = 0)


/* true when 't' is using 'dummynode' as its hash part */
// 判断表t的哈希部分是不是dummynode
#define isdummy(t)		((t)->lastfree == NULL)


/* allocated size for hash nodes */
// 获取表t哈希部分实际分配的大小 如果是dummy对象说明是0 其他情况再查询t->lsizenode
#define allocsizenode(t)	(isdummy(t) ? 0 : sizenode(t))


/* returns the key, given the value of a table entry */
// 通过Node对象中的i_val字段,查询i_key字段的地址
// offsetof用于计算结构体内某一字段与该结构体开头的偏移量
// 先计算i_val与Node的偏移量 得到Node对象地址 使用gkey获得i_key字段
#define keyfromval(v) \
  (gkey(cast(Node *, cast(char *, (v)) - offsetof(Node, i_val))))


LUAI_FUNC const TValue *luaH_getint (Table *t, lua_Integer key);
LUAI_FUNC void luaH_setint (lua_State *L, Table *t, lua_Integer key,
                                                    TValue *value);
LUAI_FUNC const TValue *luaH_getshortstr (Table *t, TString *key);
LUAI_FUNC const TValue *luaH_getstr (Table *t, TString *key);
LUAI_FUNC const TValue *luaH_get (Table *t, const TValue *key);
LUAI_FUNC TValue *luaH_newkey (lua_State *L, Table *t, const TValue *key);
LUAI_FUNC TValue *luaH_set (lua_State *L, Table *t, const TValue *key);
LUAI_FUNC Table *luaH_new (lua_State *L);
LUAI_FUNC void luaH_resize (lua_State *L, Table *t, unsigned int nasize,
                                                    unsigned int nhsize);
LUAI_FUNC void luaH_resizearray (lua_State *L, Table *t, unsigned int nasize);
LUAI_FUNC void luaH_free (lua_State *L, Table *t);
LUAI_FUNC int luaH_next (lua_State *L, Table *t, StkId key);
LUAI_FUNC int luaH_getn (Table *t);


#if defined(LUA_DEBUG)
LUAI_FUNC Node *luaH_mainposition (const Table *t, const TValue *key);
LUAI_FUNC int luaH_isdummy (const Table *t);
#endif


#endif
