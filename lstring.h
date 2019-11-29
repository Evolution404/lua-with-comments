/*
** $Id: lstring.h,v 1.61 2015/11/03 15:36:01 roberto Exp $
** String table (keep all strings handled by Lua)
** See Copyright Notice in lua.h
*/

#ifndef lstring_h
#define lstring_h

#include "lgc.h"
#include "lobject.h"
#include "lstate.h"


// TString对象需要的空间是结构体需要空间+字符串长度+1(存放\0)
// l是存放字符串的长度(不包括\0)
#define sizelstring(l)  (sizeof(union UTString) + ((l) + 1) * sizeof(char))

// Udata对象需要的空间包括 结构体占用空间+存放数据的大小
// l是Udata存放数据的大小
#define sizeludata(l)	(sizeof(union UUdata) + (l))
// u是Udata对象 计算该对象总共占用的空间 包括了实际存放的数据占用的空间
#define sizeudata(u)	sizeludata((u)->len)

// 用于创建一个明确文本的字符串 例如 luaS_newliteral(L, "hello")
#define luaS_newliteral(L, s)	(luaS_newlstr(L, "" s, \
                                 (sizeof(s)/sizeof(char))-1))


/*
** test whether a string is a reserved word
*/
// 检查一个TString对象是否是关键字
// 是短字符串且extra不为0 说明是关键字
#define isreserved(s)	((s)->tt == LUA_TSHRSTR && (s)->extra > 0)


/*
** equality for short strings, which are always internalized
*/
// 比较短字符串是否相等
// a,b是TString对象 短字符串都被内化到虚拟机内 字符串相等一定地址相等 只需要比较地址
#define eqshrstr(a,b)	check_exp((a)->tt == LUA_TSHRSTR, (a) == (b))


LUAI_FUNC unsigned int luaS_hash (const char *str, size_t l, unsigned int seed);
LUAI_FUNC unsigned int luaS_hashlongstr (TString *ts);
LUAI_FUNC int luaS_eqlngstr (TString *a, TString *b);
LUAI_FUNC void luaS_resize (lua_State *L, int newsize);
LUAI_FUNC void luaS_clearcache (global_State *g);
LUAI_FUNC void luaS_init (lua_State *L);
LUAI_FUNC void luaS_remove (lua_State *L, TString *ts);
LUAI_FUNC Udata *luaS_newudata (lua_State *L, size_t s);
LUAI_FUNC TString *luaS_newlstr (lua_State *L, const char *str, size_t l);
LUAI_FUNC TString *luaS_new (lua_State *L, const char *str);
LUAI_FUNC TString *luaS_createlngstrobj (lua_State *L, size_t l);


#endif
