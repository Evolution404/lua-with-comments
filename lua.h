/*
** $Id: lua.h,v 1.332 2016/12/22 15:51:20 roberto Exp $
** Lua - A Scripting Language
** Lua.org, PUC-Rio, Brazil (http://www.lua.org)
** See Copyright Notice at the end of this file
*/


#ifndef lua_h
#define lua_h

#include <stdarg.h>
#include <stddef.h>


#include "luaconf.h"


// Lua的一些版本信息
#define LUA_VERSION_MAJOR	"5"
#define LUA_VERSION_MINOR	"3"
#define LUA_VERSION_NUM		503
#define LUA_VERSION_RELEASE	"4"

#define LUA_VERSION	"Lua " LUA_VERSION_MAJOR "." LUA_VERSION_MINOR  // "Lua 5.3"
#define LUA_RELEASE	LUA_VERSION "." LUA_VERSION_RELEASE             // "Lua 5.3.4"
// "Lua 5.3.4  Copyright (C) 1994-2017 Lua.org, PUC-Rio"
#define LUA_COPYRIGHT	LUA_RELEASE "  Copyright (C) 1994-2017 Lua.org, PUC-Rio"
#define LUA_AUTHORS	"R. Ierusalimschy, L. H. de Figueiredo, W. Celes"


/* mark for precompiled code ('<esc>Lua') 放在预编译文件的头,用来标记预编译文件 */
#define LUA_SIGNATURE	"\x1bLua"

/* option for multiple returns in 'lua_pcall' and 'lua_call' */
// 多个返回值的标记
#define LUA_MULTRET	(-1)


/*
** Pseudo-indices
** (-LUAI_MAXSTACK is the minimum valid index; we keep some free empty
** space after that to help overflow detection)
*/
// 伪索引
#define LUA_REGISTRYINDEX	(-LUAI_MAXSTACK - 1000)
// upvalue的索引
#define lua_upvalueindex(i)	(LUA_REGISTRYINDEX - (i))


/* thread status */
// 线程的各个状态
#define LUA_OK		0
#define LUA_YIELD	1
#define LUA_ERRRUN	2
#define LUA_ERRSYNTAX	3
#define LUA_ERRMEM	4
#define LUA_ERRGCMM	5
#define LUA_ERRERR	6


typedef struct lua_State lua_State;


/*
** basic types 基本数据类型
*/
#define LUA_TNONE		(-1)

#define LUA_TNIL		0
#define LUA_TBOOLEAN		1
#define LUA_TLIGHTUSERDATA	2
#define LUA_TNUMBER		3
#define LUA_TSTRING		4
#define LUA_TTABLE		5
#define LUA_TFUNCTION		6
#define LUA_TUSERDATA		7
#define LUA_TTHREAD		8

#define LUA_NUMTAGS		9



/* minimum Lua stack available to a C function */
// 调用c函数时lua栈剩余的最小空间
#define LUA_MINSTACK	20


/* predefined values in the registry */
// G(L)->l_registry是一个具有长度为2的数组的表
// t[1]放主线程 t[2]放全局注册表
#define LUA_RIDX_MAINTHREAD	1
#define LUA_RIDX_GLOBALS 2
#define LUA_RIDX_LAST		LUA_RIDX_GLOBALS


/* type of numbers in Lua */
typedef LUA_NUMBER lua_Number;      // double


/* type for integer functions */
typedef LUA_INTEGER lua_Integer;    // long long

/* unsigned integer type */
typedef LUA_UNSIGNED lua_Unsigned;  // unsigned long long

/* type for continuation-function contexts */
typedef LUA_KCONTEXT lua_KContext;  // intptr_t


/*
** Type for C functions registered with Lua
*/
// c函数
typedef int (*lua_CFunction) (lua_State *L);

/*
** Type for continuation functions
*/
// 延续函数
typedef int (*lua_KFunction) (lua_State *L, int status, lua_KContext ctx);


/*
** Type for functions that read/write blocks when loading/dumping Lua chunks
*/
// 用于读取和写入lua块的函数,只有一次用到其实就是getF函数
typedef const char * (*lua_Reader) (lua_State *L, void *ud, size_t *sz);

typedef int (*lua_Writer) (lua_State *L, const void *p, size_t sz, void *ud);


/*
** Type for memory-allocation functions
*/
// 内存分配函数
typedef void * (*lua_Alloc) (void *ud, void *ptr, size_t osize, size_t nsize);



/*
** generic extra include file
*/
// 用于用户引入头文件
#if defined(LUA_USER_H)
#include LUA_USER_H
#endif


/*
** RCS ident string
*/
extern const char lua_ident[];


/*
** state manipulation
** 线程操作
*/
// 创建线程
LUA_API lua_State *(lua_newstate) (lua_Alloc f, void *ud);
// 销毁线程
LUA_API void       (lua_close) (lua_State *L);
// 创建线程
LUA_API lua_State *(lua_newthread) (lua_State *L);

LUA_API lua_CFunction (lua_atpanic) (lua_State *L, lua_CFunction panicf);


LUA_API const lua_Number *(lua_version) (lua_State *L);


/*
** basic stack manipulation
** 基本栈操作
*/
// 将其他格式idx转换成相对于L->ci->func的位置
LUA_API int   (lua_absindex) (lua_State *L, int idx);
// 返回top-1位置的idx
LUA_API int   (lua_gettop) (lua_State *L);
// 修改top指向
// 正数: top = func+1+idx  传入0 top指向func+1
// 负数: top = top+1+idx   传入-1 top指向top 传入-2 top指向top-1
LUA_API void  (lua_settop) (lua_State *L, int idx);
// 将idx位置值压入栈顶
LUA_API void  (lua_pushvalue) (lua_State *L, int idx);
// 将从idx开始到栈顶的值进行左移(n>0)或右移(n<0)
LUA_API void  (lua_rotate) (lua_State *L, int idx, int n);
// 设置toidx的值位fromidx
LUA_API void  (lua_copy) (lua_State *L, int fromidx, int toidx);
// 确保栈上至少还有n个空间 成功返回1 失败返回0
LUA_API int   (lua_checkstack) (lua_State *L, int n);

// 从from的栈顶上移动n个值到to的栈上 from栈顶下降n
LUA_API void  (lua_xmove) (lua_State *from, lua_State *to, int n);


/*
** access functions (stack -> C)
*/

LUA_API int             (lua_isnumber) (lua_State *L, int idx);
LUA_API int             (lua_isstring) (lua_State *L, int idx);
LUA_API int             (lua_iscfunction) (lua_State *L, int idx);
LUA_API int             (lua_isinteger) (lua_State *L, int idx);
LUA_API int             (lua_isuserdata) (lua_State *L, int idx);
LUA_API int             (lua_type) (lua_State *L, int idx);
LUA_API const char     *(lua_typename) (lua_State *L, int tp);

LUA_API lua_Number      (lua_tonumberx) (lua_State *L, int idx, int *isnum);
LUA_API lua_Integer     (lua_tointegerx) (lua_State *L, int idx, int *isnum);
LUA_API int             (lua_toboolean) (lua_State *L, int idx);
LUA_API const char     *(lua_tolstring) (lua_State *L, int idx, size_t *len);
LUA_API size_t          (lua_rawlen) (lua_State *L, int idx);
LUA_API lua_CFunction   (lua_tocfunction) (lua_State *L, int idx);
LUA_API void	       *(lua_touserdata) (lua_State *L, int idx);
LUA_API lua_State      *(lua_tothread) (lua_State *L, int idx);
LUA_API const void     *(lua_topointer) (lua_State *L, int idx);


/*
** Comparison and arithmetic functions
*/
/* ORDER TM, ORDER OP */
#define LUA_OPADD	0    // 加法(+)
#define LUA_OPSUB	1    // 减法(-)
#define LUA_OPMUL	2    // 乘法(*)
#define LUA_OPMOD	3    // 取模(%)
#define LUA_OPPOW	4    // 乘方(^)
#define LUA_OPDIV	5    // 向下取整的除法(//)
#define LUA_OPIDIV	6  // 浮点除法(/)
#define LUA_OPBAND	7  // 按位与(&)
#define LUA_OPBOR	8    // 按位或(|)
#define LUA_OPBXOR	9  // 按位异或(~)
#define LUA_OPSHL	10   // 左移(<<)
#define LUA_OPSHR	11   // 右移(>>)
#define LUA_OPUNM	12   // 取负(一元-)
#define LUA_OPBNOT	13 // 按位取反(~)

// 将栈顶的1个或2个操作数执行op运算 删除操作数栈顶保留运算结果
LUA_API void  (lua_arith) (lua_State *L, int op);

#define LUA_OPEQ	0
#define LUA_OPLT	1
#define LUA_OPLE	2

// 比较index1和index2上的值是否相等 不查询元表
LUA_API int   (lua_rawequal) (lua_State *L, int idx1, int idx2);
// 对index1和index2的值进行相等(LUA_OPEQ),小于(LUA_OPLT),小于等于(LUA_OPLE)的比较
LUA_API int   (lua_compare) (lua_State *L, int idx1, int idx2, int op);


/*
** push functions (C -> stack)
*/
// 向栈顶压入不同类型的数据
LUA_API void        (lua_pushnil) (lua_State *L);                                // nil
LUA_API void        (lua_pushnumber) (lua_State *L, lua_Number n);               // 浮点数
LUA_API void        (lua_pushinteger) (lua_State *L, lua_Integer n);             // 整数
LUA_API const char *(lua_pushlstring) (lua_State *L, const char *s, size_t len); // 指定长度字符串
LUA_API const char *(lua_pushstring) (lua_State *L, const char *s);              // 字符串
LUA_API const char *(lua_pushvfstring) (lua_State *L, const char *fmt,           // 格式字符串
                                                      va_list argp);
LUA_API const char *(lua_pushfstring) (lua_State *L, const char *fmt, ...);      // 格式字符串(可变参数形式)
LUA_API void  (lua_pushcclosure) (lua_State *L, lua_CFunction fn, int n);        // c闭包
LUA_API void  (lua_pushboolean) (lua_State *L, int b);                           // 布尔值
LUA_API void  (lua_pushlightuserdata) (lua_State *L, void *p);                   // 轻量级用户数据
LUA_API int   (lua_pushthread) (lua_State *L);                                   // 线程


/*
** get functions (Lua -> stack)
*/
// get函数返回值都是查询结果的类型

// 查询_G[name]的值并压入栈顶
LUA_API int (lua_getglobal) (lua_State *L, const char *name);
// t为idx位置的表
// top-1=t[top-1] 修改栈顶的值为表中项,栈顶不变
LUA_API int (lua_gettable) (lua_State *L, int idx);
// 查询idx位置值的k键的值 压入栈顶,栈顶上升
LUA_API int (lua_getfield) (lua_State *L, int idx, const char *k);
// 设置top-1 = t[n],栈顶上升
LUA_API int (lua_geti) (lua_State *L, int idx, lua_Integer n);
// top-1=t[top-1] 不查询元表,栈顶不变
LUA_API int (lua_rawget) (lua_State *L, int idx);
// top-1 = t[n] 查询结果压入栈顶,不查询元表,栈顶上升
LUA_API int (lua_rawgeti) (lua_State *L, int idx, lua_Integer n);
// top-1 = t[p] 查询结果压入栈顶,不查询元表,栈顶上升
LUA_API int (lua_rawgetp) (lua_State *L, int idx, const void *p);

// 新建一个table对象放到栈顶
LUA_API void  (lua_createtable) (lua_State *L, int narr, int nrec);
// 创建一个userdata压入栈顶 返回userdata的数据实际存放位置
LUA_API void *(lua_newuserdata) (lua_State *L, size_t sz);
// 获取objindex位置的元表 将获取到元表压入栈顶
LUA_API int   (lua_getmetatable) (lua_State *L, int objindex);
// 将idx位置用户数据压入栈顶
LUA_API int  (lua_getuservalue) (lua_State *L, int idx);


/*
** set functions (stack -> Lua)
*/
// 设置 _G[name] = 栈顶的值 弹出栈顶
LUA_API void  (lua_setglobal) (lua_State *L, const char *name);
// t[top-2]=top-1 弹出 top-2和top-1
LUA_API void  (lua_settable) (lua_State *L, int idx);
// t[k]=top-1 弹出top-1
LUA_API void  (lua_setfield) (lua_State *L, int idx, const char *k);
// t[n]=top-1 弹出top-1
LUA_API void  (lua_seti) (lua_State *L, int idx, lua_Integer n);
// t[top-2]=top-1 弹出top-2和top-1 不查询元表
LUA_API void  (lua_rawset) (lua_State *L, int idx);
// t[n]=top-1 弹出栈顶 不查询元表
LUA_API void  (lua_rawseti) (lua_State *L, int idx, lua_Integer n);
// t[p]=top-1 弹出栈顶 不查询元表
LUA_API void  (lua_rawsetp) (lua_State *L, int idx, const void *p);
// 设置objindex位置的值的元表为栈顶值,弹出栈顶
LUA_API int   (lua_setmetatable) (lua_State *L, int objindex);
// 设置idx位置k键的值为当前栈顶的值 弹出栈顶
LUA_API void  (lua_setuservalue) (lua_State *L, int idx);


/*
** 'load' and 'call' functions (load and run Lua code)
*/
LUA_API void  (lua_callk) (lua_State *L, int nargs, int nresults,
                           lua_KContext ctx, lua_KFunction k);
#define lua_call(L,n,r)		lua_callk(L, (n), (r), 0, NULL)

LUA_API int   (lua_pcallk) (lua_State *L, int nargs, int nresults, int errfunc,
                            lua_KContext ctx, lua_KFunction k);
#define lua_pcall(L,n,r,f)	lua_pcallk(L, (n), (r), (f), 0, NULL)

LUA_API int   (lua_load) (lua_State *L, lua_Reader reader, void *dt,
                          const char *chunkname, const char *mode);

LUA_API int (lua_dump) (lua_State *L, lua_Writer writer, void *data, int strip);


/*
** coroutine functions
*/
LUA_API int  (lua_yieldk)     (lua_State *L, int nresults, lua_KContext ctx,
                               lua_KFunction k);
LUA_API int  (lua_resume)     (lua_State *L, lua_State *from, int narg);
LUA_API int  (lua_status)     (lua_State *L);
LUA_API int (lua_isyieldable) (lua_State *L);

#define lua_yield(L,n)		lua_yieldk(L, (n), 0, NULL)


/*
** garbage-collection function and options
*/

#define LUA_GCSTOP		0
#define LUA_GCRESTART		1
#define LUA_GCCOLLECT		2
#define LUA_GCCOUNT		3
#define LUA_GCCOUNTB		4
#define LUA_GCSTEP		5
#define LUA_GCSETPAUSE		6
#define LUA_GCSETSTEPMUL	7
#define LUA_GCISRUNNING		9

LUA_API int (lua_gc) (lua_State *L, int what, int data);


/*
** miscellaneous functions
*/

LUA_API int   (lua_error) (lua_State *L);

LUA_API int   (lua_next) (lua_State *L, int idx);

LUA_API void  (lua_concat) (lua_State *L, int n);
LUA_API void  (lua_len)    (lua_State *L, int idx);

LUA_API size_t   (lua_stringtonumber) (lua_State *L, const char *s);

LUA_API lua_Alloc (lua_getallocf) (lua_State *L, void **ud);
LUA_API void      (lua_setallocf) (lua_State *L, lua_Alloc f, void *ud);



/*
** {==============================================================
** some useful macros
** ===============================================================
*/

#define lua_getextraspace(L)	((void *)((char *)(L) - LUA_EXTRASPACE))

#define lua_tonumber(L,i)	lua_tonumberx(L,(i),NULL)
#define lua_tointeger(L,i)	lua_tointegerx(L,(i),NULL)

// 将top向前移动n个
#define lua_pop(L,n)		lua_settop(L, -(n)-1)

#define lua_newtable(L)		lua_createtable(L, 0, 0)

#define lua_register(L,n,f) (lua_pushcfunction(L, (f)), lua_setglobal(L, (n)))

#define lua_pushcfunction(L,f)	lua_pushcclosure(L, (f), 0)

#define lua_isfunction(L,n)	(lua_type(L, (n)) == LUA_TFUNCTION)
#define lua_istable(L,n)	(lua_type(L, (n)) == LUA_TTABLE)
#define lua_islightuserdata(L,n)	(lua_type(L, (n)) == LUA_TLIGHTUSERDATA)
#define lua_isnil(L,n)		(lua_type(L, (n)) == LUA_TNIL)
#define lua_isboolean(L,n)	(lua_type(L, (n)) == LUA_TBOOLEAN)
#define lua_isthread(L,n)	(lua_type(L, (n)) == LUA_TTHREAD)
#define lua_isnone(L,n)		(lua_type(L, (n)) == LUA_TNONE)
#define lua_isnoneornil(L, n)	(lua_type(L, (n)) <= 0)

#define lua_pushliteral(L, s)	lua_pushstring(L, "" s)

#define lua_pushglobaltable(L)  \
	((void)lua_rawgeti(L, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS))

#define lua_tostring(L,i)	lua_tolstring(L, (i), NULL)


#define lua_insert(L,idx)	lua_rotate(L, (idx), 1)

#define lua_remove(L,idx)	(lua_rotate(L, (idx), -1), lua_pop(L, 1))

#define lua_replace(L,idx)	(lua_copy(L, -1, (idx)), lua_pop(L, 1))

/* }============================================================== */


/*
** {==============================================================
** compatibility macros for unsigned conversions
** ===============================================================
*/
#if defined(LUA_COMPAT_APIINTCASTS)

#define lua_pushunsigned(L,n)	lua_pushinteger(L, (lua_Integer)(n))
#define lua_tounsignedx(L,i,is)	((lua_Unsigned)lua_tointegerx(L,i,is))
#define lua_tounsigned(L,i)	lua_tounsignedx(L,(i),NULL)

#endif
/* }============================================================== */

/*
** {======================================================================
** Debug API
** =======================================================================
*/


/*
** Event codes
*/
#define LUA_HOOKCALL	0
#define LUA_HOOKRET	1
#define LUA_HOOKLINE	2
#define LUA_HOOKCOUNT	3
#define LUA_HOOKTAILCALL 4


/*
** Event masks
*/
#define LUA_MASKCALL	(1 << LUA_HOOKCALL)
#define LUA_MASKRET	(1 << LUA_HOOKRET)
#define LUA_MASKLINE	(1 << LUA_HOOKLINE)
#define LUA_MASKCOUNT	(1 << LUA_HOOKCOUNT)

typedef struct lua_Debug lua_Debug;  /* activation record */


/* Functions to be called by the debugger in specific events */
typedef void (*lua_Hook) (lua_State *L, lua_Debug *ar);


LUA_API int (lua_getstack) (lua_State *L, int level, lua_Debug *ar);
LUA_API int (lua_getinfo) (lua_State *L, const char *what, lua_Debug *ar);
LUA_API const char *(lua_getlocal) (lua_State *L, const lua_Debug *ar, int n);
LUA_API const char *(lua_setlocal) (lua_State *L, const lua_Debug *ar, int n);
LUA_API const char *(lua_getupvalue) (lua_State *L, int funcindex, int n);
LUA_API const char *(lua_setupvalue) (lua_State *L, int funcindex, int n);

LUA_API void *(lua_upvalueid) (lua_State *L, int fidx, int n);
LUA_API void  (lua_upvaluejoin) (lua_State *L, int fidx1, int n1,
                                               int fidx2, int n2);

LUA_API void (lua_sethook) (lua_State *L, lua_Hook func, int mask, int count);
LUA_API lua_Hook (lua_gethook) (lua_State *L);
LUA_API int (lua_gethookmask) (lua_State *L);
LUA_API int (lua_gethookcount) (lua_State *L);


struct lua_Debug {
  int event;
  const char *name;	/* (n) */
  const char *namewhat;	/* (n) 'global', 'local', 'field', 'method' */
  const char *what;	/* (S) 'Lua', 'C', 'main', 'tail' */
  const char *source;	/* (S) */
  int currentline;	/* (l) */
  int linedefined;	/* (S) */
  int lastlinedefined;	/* (S) */
  unsigned char nups;	/* (u) number of upvalues */
  unsigned char nparams;/* (u) number of parameters */
  char isvararg;        /* (u) */
  char istailcall;	/* (t) */
  char short_src[LUA_IDSIZE]; /* (S) */
  /* private part */
  struct CallInfo *i_ci;  /* active function */
};

/* }====================================================================== */


/******************************************************************************
* Copyright (C) 1994-2017 Lua.org, PUC-Rio.
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************/


#endif
